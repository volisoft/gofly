package fly

import java.time.{LocalDate, ZonedDateTime}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}

import com.google.common.collect.Range
import rapture.json.Json

import scala.io.Source
import rapture.json._
import jsonBackends.jawn._
import rapture.uri._
import rapture.io._
import rapture.net._
import rapture.codec._
import encodings.`UTF-8`._
import org.jsoup.Jsoup
import rapture.data.DataContext

import scala.collection.immutable.Seq

/**
  * Created by dev on 2/28/17.
  */
object AllegiantApi {
  private val ISO_DATETIME_FORMATTER = new DateTimeFormatterBuilder().parseCaseInsensitive()
    .append(DateTimeFormatter.ISO_DATE_TIME).appendPattern("X").toFormatter

  trait AP {
    val name: String
    val fullName: String
    val code: String
  }
  case class Airport(name: String, full_name: String, code: String,
                     connections: Map[String, Boolean], connections_iata_codes: Map[String, String]) {
    def connections0: List[String] = connections.keys.toList
    def connectionsCodes: List[String] = connections_iata_codes.values.toList
  }

  case class Flight(origin: String, destination: String, departs: ZonedDateTime, arrives: ZonedDateTime,
                    flight_no: String, airline_code: String, price: Int, availability: Int)

  case class Segment(from: String, to: String, departAt: LocalDate, arriveAt: LocalDate, price: Int)


  def dates(from: String, to: String): (Map[String, List[LocalDate]], Range[LocalDate]) = {
    implicit val dateExtractor = Json.extractor[String].map(LocalDate.parse(_))
    val dates: HttpUrl = uri"https://www.allegiantair.com/g4search/api/flight/calendar/$from/$to"
    val datesData = dates.httpGet().slurp[Char]

    val (datesMap, validFrom, validTo) = Json.parse(datesData) match {
      case json""" { "availableDates" : $datesObj,
                     "market": { "validFrom": $validFrom, "validTo": $validTo }
                   } """ => (datesObj.as[Map[String, List[LocalDate]]], validFrom.as[LocalDate], validTo.as[LocalDate])
    }
    (datesMap, Range.closed(validFrom, validTo))
  }

  def availableFlights(from: String, to: String, departAt: LocalDate, returnAt: LocalDate): List[Flight] = {
    val searchRequest = s"""{ "outward": "${DateTimeFormatter.ISO_DATE.format(departAt)}",
                               "returning": "${DateTimeFormatter.ISO_DATE.format(returnAt)}",
                               "destination_code": "$to",
                               "origin_code": "$from",
                               "travelers": {
                                 "adult":1,
                                 "child_dobs":[],
                                 "lapchild_dobs":[]
                               }
                             }""".stripMargin.replaceAll("\n", " ")

    val searchAttributesJson = Jsoup.connect("https://www.allegiantair.com/g4search/api/booking/search").requestBody(searchRequest).post().body().text()

    val departingUri = Json.parse(searchAttributesJson) match {
      case json""" { "id": $id,
                     "silo_id": $siloId,
                     "flights": {
                       "departing": $departingUri0,
                       "returning": $returningUri0
                     }
                   } """ => departingUri0.as[String] // departing -> /data/siloId/id-fromto; returning -> /data/siloId/id-tofrom
    }

    val departUrl: HttpUrl = uri"https://www.allegiantair.com$departingUri"
    val departContent = departUrl.httpGet().slurp[Char]

    val flights = Json.parse(departContent).as[List[Json]].map(extractFlight)
    flights
  }

  def extractFlight(json: Json): Flight = json match {
    case json"""
         {
           "origin": $origin,
           "destination": $destination,
           "arrives": $arrives,
           "departs": $departs,
           "flight_no": $flightNo,
           "airline_code": $airlineCode,
           "seat_price": [ {
             "pp": {
               "value": $price
             },
             "availability": $availability
           } ]
         }""" =>
      Flight(origin.as[String], destination.as[String],
        ZonedDateTime.parse(departs.as[String], ISO_DATETIME_FORMATTER),
        ZonedDateTime.parse(arrives.as[String], ISO_DATETIME_FORMATTER),
        flightNo.as[String],
        airlineCode.as[String],
        price.as[Int],
        availability.as[Int])
  }

  def isValidItinerary(itinerary: List[Flight]): Boolean = itinerary match {
    case Nil => true
    case x :: Nil => true
    case x :: y :: xs => (x.arrives isBefore y.departs) && isValidItinerary(y :: xs)
  }

  def itineraries(path: List[String], departAt: LocalDate, returnAt: LocalDate): List[List[Flight]] = {
    val sections = path.reverse.sliding(2).map { case from :: to :: Nil => availableFlights(from, to, departAt, returnAt) }.toList.reverse
    val its = sections.foldLeft(List(List[Flight]())) { (acc, flights) =>
        for {
          itinerary <- acc
          flight <- flights
        } yield flight :: itinerary
      }
      .filter(isValidItinerary)
    its
  }


  def main(args: Array[String]): Unit = {
    val json = Json parse Source
              .fromFile("/home/dev/workspace/gofly/src/main/resources/allegiant-search-form.json").getLines().mkString
    val (airports, graph) = createGraph(json)

    val from = ", ut"
    val to = "tampa"
    val stops = 3

    val paths = findPaths(from, to, airports, graph, stops)

//    val (datesMap, validityRange) = dates("PVU", "OAK")
//    availableFlights("PVU", "OAK", LocalDate.parse("2017-03-08"), LocalDate.parse("2017-03-12"))

    val its = for {
      path <- paths
      itinerary <- itineraries(path, LocalDate.parse("2017-05-21"), LocalDate.parse("2017-05-28"))
    } yield itinerary
    println(its)
  }

  def createGraph(json: Json): (List[Airport], Digraph[String]) = {
    val airports = json.airports.as[Map[String, Airport]].values.toList
    val digraph = airports.foldLeft[Digraph[String]](EmptyDigraph()) {
      case (graph, airport) =>
        airport.connectionsCodes.foldLeft(graph)((g, conn) =>
          g.addEdge(airport.code, conn))
    }
    (airports, digraph)
  }

  def findPaths(from: String, to: String, airports: List[Airport], airportsGraph: Digraph[String], stops: Int): Iterable[List[String]] = {
    val fromAirPorts = airports.filter(_.name.toLowerCase.contains(from)).map(_.code)
    val destination = airports.filter(_.name.toLowerCase.contains(to)).map(_.code)

    val paths = for {
      from0 <- fromAirPorts
      path0 <- pathsTailRecursive(airportsGraph, from0, stops) if destination.contains(path0.head)
    } yield path0
    paths
  }

}
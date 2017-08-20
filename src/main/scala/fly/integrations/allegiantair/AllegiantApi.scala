package fly.integrations.allegiantair

import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.{LocalDate, ZonedDateTime}

import com.google.common.collect.Range
import fly.alg.{Digraph, EmptyDigraph}
import fly.domain._
import org.jsoup.{Connection, Jsoup}
import rapture.io._
import rapture.json.{Json, _}
import rapture.json.jsonBackends.jawn._
import rapture.net._
import rapture.uri._

/**
  * Created by dev on 2/28/17.
  */
object AllegiantApi {
  private val ISO_DATETIME_FORMATTER = new DateTimeFormatterBuilder().parseCaseInsensitive()
    .append(DateTimeFormatter.ISO_DATE_TIME).appendPattern("X").toFormatter

  type ValidityPeriod = Range[LocalDate]

  private def dates(from: String, to: String): (Map[String, List[LocalDate]], ValidityPeriod) = {
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

    val preSearchResponse = Jsoup.connect("https://www.allegiantair.com/g4search/api/booking/search")
      .ignoreContentType(true)
      .requestBody(searchRequest)
      .method(Connection.Method.POST)
      .followRedirects(false)
      .execute()

    val manifestId = preSearchResponse.header("manifest-id")
    val siloId = preSearchResponse.header("silo-id")

    val searchAttributesJson = Jsoup.connect(s"https://www.allegiantair.com/data/$siloId/$manifestId?id=$manifestId")
      .ignoreContentType(true)
      .get().body().text()

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

  private def extractFlight(json: Json): Flight = json match {
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

  def createGraph(json: Json): (List[Airport], Digraph[String]) = {
    val airports = json.airports.as[Map[String, Airport]].values.toList
    val digraph = airports.foldLeft[Digraph[String]](EmptyDigraph()) {
      case (graph, airport) =>
        airport.connectionsCodes.foldLeft(graph)((g, conn) =>
          g.addEdge(airport.code, conn))
    }
    (airports, digraph)
  }
}
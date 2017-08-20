package fly

import java.time.temporal.ChronoUnit
import java.time.{LocalDate, ZonedDateTime}

import fly.integrations.allegiantair.AllegiantApi.availableFlights
import fly.alg.{Digraph, Paths, pathsTailRecursive}

package object domain {
  case class Airport(name: String, full_name: String, code: String,
                     connections: Map[String, Boolean], connections_iata_codes: Map[String, String]) {
    def connections0: List[String] = connections.keys.toList
    def connectionsCodes: List[String] = connections_iata_codes.values.toList
  }

  case class Flight(origin: String, destination: String, departs: ZonedDateTime, arrives: ZonedDateTime,
                    flight_no: String, airline_code: String, price: Int, availability: Int) {
    override def toString: String = s"[$origin ($departs) -> $destination($arrives)]"
  }

  case class Segment(from: String, to: String, departAt: LocalDate, arriveAt: LocalDate, price: Int)

  case class CodeIATA(code: String) {
    assert(code.length == 3)
  }

  private def isValidItinerary(itinerary: List[Flight]): Boolean = itinerary match {
    case Nil => true
    case _ :: Nil => true
    case x :: y :: xs => (x.arrives isBefore y.departs) && isValidItinerary(y :: xs)
  }

  def itineraries(path: List[String], departAt: LocalDate, returnAt: LocalDate): List[List[Flight]] = {
    val sections = path.reverse.sliding(2).map {
      case from :: to :: Nil => availableFlights(from, to, departAt, returnAt)
    }.toList.reverse

    val its = sections.foldLeft(List(List[Flight]())) { (acc, flights) =>
      for {
        itinerary <- acc
        flight <- flights
      } yield flight :: itinerary
    }
      .filter(isValidItinerary)
    its
  }

  def findPaths(from: String, to: String, airports: List[Airport], airportsGraph: Digraph[String], stops: Int): Paths[String] = {
    val fromAirPorts = airports.filter(_.name.toLowerCase.contains(from)).map(_.code)
    val destination = airports.filter(_.name.toLowerCase.contains(to)).map(_.code)

    val paths = for {
      from0 <- fromAirPorts
      path0 <- pathsTailRecursive(airportsGraph, from0, stops) if destination.contains(path0.head)
    } yield path0
    paths
  }


  def show(itinerary: List[Flight]): String = {
    val route = itinerary.foldLeft(itinerary.head.origin)((path, connection) => s"$path -> ${connection.destination}")
    val price = itinerary.foldLeft(0)(_ + _.price)
    val duration = itinerary.head.departs.until(itinerary.last.arrives, ChronoUnit.MINUTES)
    val departAt = itinerary.head.departs
    val arriveAt = itinerary.last.arrives

    s"$$$price | ${duration/60} hrs ${duration%60} min | $route | $departAt -> $arriveAt"
  }

}

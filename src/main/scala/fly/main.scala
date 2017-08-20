package fly

import java.time.LocalDate

import fly.domain.{findPaths, itineraries, show}
import fly.integrations.allegiantair.AllegiantApi
import rapture.json.Json

import scala.io.Source

object main {
  def main(args: Array[String]): Unit = {
    val json = Json parse Source
      .fromFile("/Users/user/workspace/delete/gofly/src/main/resources/allegiant-search-form-9aug2017.json")
      .getLines().mkString

    val (airports, graph) = AllegiantApi.createGraph(json)

    val from = ", ut"
    val to = "tampa"
    val stops = 3

    val paths = findPaths(from, to, airports, graph, stops)

    val its = for {
      path <- paths
      itinerary <- itineraries(path, LocalDate.parse("2017-08-21"), LocalDate.parse("2017-08-28"))
    } yield itinerary

    its.foreach(i => println(show(i)))
  }

}

import java.time.temporal.{ChronoUnit}

import fly.AllegiantApi.Flight

/**
  * Created by dev on 3/3/17.
  */
package object fly {
  abstract class Digraph[T] {
    def addEdge(v: T, w: T): Digraph[T]
    def adj(v: T): Iterable[T]
    def indegree(v: T): Int
    def outdegree(v: T): Int
    def V: Int
  }

  case class EmptyDigraph[T]() extends Digraph[T] {
    override def addEdge(v: T, w: T): Digraph[T] = NonemptyDigraph[T](Map(v -> Set(w)), E = 1, indegree0 = Map(w -> 1))

    override def adj(v: T): Iterable[T] = throw new Error("Empty.adj")

    override def indegree(v: T): Int = 0

    override def outdegree(v: T): Int = 0

    override def V: Int = 0
  }

  case class NonemptyDigraph[T](adjacency: Map[T, Set[T]], E: Int, indegree0: Map[T, Int]) extends Digraph[T] {
    override def addEdge(v: T, w: T): Digraph[T] = {
      val adjacent = adjacency.getOrElse(v, Set())
      val vw = adjacent + w
      val degree = 1 + indegree0.getOrElse(w, 0)
      NonemptyDigraph(adjacency + (v -> vw), E + 1, indegree0 + (w -> degree))
    }

    override def adj(v: T): Iterable[T] = adjacency.getOrElse(v, Nil)

    override def indegree(v: T): Int = indegree0(v)

    override def outdegree(v: T): Int = adjacency.getOrElse(v, List()).size

    override def V: Int = E + 1
  }

  case class EmptyDigraphList[T]() extends Digraph[T] {
    override def addEdge(v: T, w: T): Digraph[T] = NonemptyDigraphList[T](Map(v -> List(w)), E = 1, indegree0 = Map(w -> 1))

    override def adj(v: T): Iterable[T] = throw new Error("Empty.adj")

    override def indegree(v: T): Int = 0

    override def outdegree(v: T): Int = 0

    override def V: Int = 0
  }

  case class NonemptyDigraphList[T](adjacency: Map[T, List[T]], E: Int, indegree0: Map[T, Int]) extends Digraph[T] {
    override def addEdge(v: T, w: T): Digraph[T] = {
      val adjacent = adjacency.getOrElse(v, List())
      val vw = w :: adjacent
      val degree = 1 + indegree0.getOrElse(w, 0)
      NonemptyDigraphList(adjacency + (v -> vw), E + 1, indegree0 + (w -> degree))
    }

    override def adj(v: T): Iterable[T] = adjacency.getOrElse(v, Nil)

    override def indegree(v: T): Int = indegree0(v)

    override def outdegree(v: T): Int = adjacency.getOrElse(v, List()).size

    override def V: Int = E + 1
  }

  def pathsTailRecursive[T](G: Digraph[T], source: T, depth: Int): List[List[T]] = {
    type Paths = List[List[T]]
    def loop(acc: Paths, toVisit: Paths)(implicit G: Digraph[T]): Paths = toVisit match {
      case Nil => acc
      case path :: xs =>
        val next = for (v <- G.adj(path.head) if !path.contains(v) && path.size <= depth) yield v :: path
        loop(path :: acc, next.toList ++ xs)
    }
    loop(List(), List(List(source)))(G)
  }

  def dfsRecursive[T](G: Digraph[T], source: T): Iterable[Iterable[T]] = {
    def loop(G: Digraph[T], sources: Iterable[T], visited: Set[T]): Iterable[Iterable[T]] = sources match {
      case Nil => List(List())
      case x :: xs =>
        val paths = for {
          subpath <- loop(G, G.adj(x).filterNot(visited.contains) , visited + x)
        } yield x :: subpath.toList
        paths ++ loop(G, xs, visited)
    }

    loop(G, List(source), Set())
  }

  def show(itinerary: List[Flight]): String = {
    val route = itinerary.foldLeft(itinerary.head.origin)((path, connection) => s"$path -> ${connection.destination}")
    val price = itinerary.foldLeft(0)(_ + _.price)
    val duration = itinerary.foldLeft(0L)((z, conn) => z + conn.departs.until(conn.arrives, ChronoUnit.HOURS))
    val departAt = itinerary.head.departs
    val arriveAt = itinerary.last.arrives

    s"$$$price | $duration hrs | $route | depart at: $departAt, arrive at: $arriveAt"
  }
}
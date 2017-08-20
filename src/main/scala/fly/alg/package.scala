package fly

/**
  * Created by dev on 3/3/17.
  */
package object alg {
  abstract class Digraph[T] {
    def addEdge(v: T, w: T): Digraph[T]
    def adj(v: T): Iterable[T]
    def indegree(v: T): Int
    def outdegree(v: T): Int
    def vertices: Int
    def contains(v: T): Boolean
  }

  case class EmptyDigraph[T]() extends Digraph[T] {
    override def addEdge(v: T, w: T): Digraph[T] = NonemptyDigraph(Map(v -> Set(w)), E = 1, indegree0 = Map(w -> 1))

    override def adj(v: T): Iterable[T] = List()

    override def indegree(v: T): Int = 0

    override def outdegree(v: T): Int = 0

    override def vertices: Int = 0

    override def contains(v: T): Boolean = false
  }

  case class NonemptyDigraph[T](adjacency: Map[T, Set[T]], E: Int, indegree0: Map[T, Int]) extends Digraph[T] {
    override def addEdge(v: T, w: T): Digraph[T] = {
      val adjacent = adjacency.getOrElse(v, Set())
      val vw = adjacent + w
      val degree = 1 + indegree0.getOrElse(w, 0)
      NonemptyDigraph(adjacency + (v -> vw), E + 1, indegree0 + (w -> degree))
    }

    override def adj(v: T): Iterable[T] = adjacency.getOrElse(v, Nil)

    override def indegree(v: T): Int = indegree0.getOrElse(v, 0)

    override def outdegree(v: T): Int = adjacency.getOrElse(v, List()).size

    override def vertices: Int = E + 1

    override def contains(v: T): Boolean = indegree0.contains(v) || adjacency.contains(v)
  }

  case class EmptyDigraphList[T]() extends Digraph[T] {
    override def addEdge(v: T, w: T): Digraph[T] = NonemptyDigraphList[T](Map(v -> List(w)), E = 1, indegree0 = Map(w -> 1))

    override def adj(v: T): Iterable[T] = throw new Error("Empty.adj")

    override def indegree(v: T): Int = 0

    override def outdegree(v: T): Int = 0

    override def vertices: Int = 0

    override def contains(v: T): Boolean = false
  }

  case class NonemptyDigraphList[T](adjacency: Map[T, List[T]], E: Int, indegree0: Map[T, Int]) extends Digraph[T] {
    override def addEdge(v: T, w: T): Digraph[T] = {
      val adjacent = adjacency.getOrElse(v, List())
      val vw = w :: adjacent
      val degree = 1 + indegree0.getOrElse(w, 0)
      NonemptyDigraphList(adjacency + (v -> vw), E + 1, indegree0 + (w -> degree))
    }

    override def adj(v: T): Iterable[T] = adjacency.getOrElse(v, List())

    override def indegree(v: T): Int = indegree0(v)

    override def outdegree(v: T): Int = adj(v).size

    override def vertices: Int = E + 1

    override def contains(v: T): Boolean = indegree0.contains(v) || adjacency.contains(v)
  }

  type Paths[T] = List[List[T]]

  def pathsTailRecursive[T](G: Digraph[T], source: T, length: Int): Paths[T] = {
    def loop(acc: Paths[T], toVisit: Paths[T])(implicit G: Digraph[T]): Paths[T] = toVisit match {
      case Nil => acc
      case path :: xs =>
        val next = for (v <- G.adj(path.head) if !path.contains(v) && path.size <= length) yield v :: path
        loop(path :: acc, next.toList ++ xs)
    }

    val initialPaths = G.adj(source).map(List(_, source)).toList
    loop(List(), initialPaths)(G)
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
}
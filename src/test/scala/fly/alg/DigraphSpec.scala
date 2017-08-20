package fly.alg

import fly.UnitSpec

import scala.collection.immutable

class DigraphSpec extends UnitSpec {

  implicit class DSL[T](v: T) {
    def ~>(w: T): Digraph[T] = EmptyDigraph().addEdge(v, w)
  }
  case class GraphDSL[T](v: T, g: Digraph[T]) {
    def ~>(w: T): Digraph[T] = g.addEdge(v, w)
  }

  private def emptyDigraph[T] = EmptyDigraph[T]()

  private val Array(a_, b, c, d, e) = Array('a', 'b', 'c', 'd', 'e')

  private def paths[T](g: Digraph[T], source: T)(implicit depth: Int = 5) = pathsTailRecursive(g, source, depth)
  // a -> b
  private val graphSingleEdge = emptyDigraph[Char].addEdge(a_, b)

  // a -> b -> c
  private val graph2Linear = emptyDigraph[Char].addEdge(a_, b).addEdge(b, c)

  /**
    a->b->c->d
     ⤷     ⤴
  */
  private val graph4Tree = emptyDigraph[Char].addEdge(a_, b).addEdge(b, c).addEdge(c, d).addEdge(a_, d)

  /** a->d<-c
      ⤷ b ⤴
  */
  private val graph4TreeAlt = emptyDigraph[Char].addEdge(a_, d).addEdge(c, d).addEdge(b, c).addEdge(a_, b)

  /** a⟺d<-c
      ⤷ b ⤴
  */
  private val graph4TreeCycled = emptyDigraph[Char]
    .addEdge(a_, d)
    .addEdge(c, d)
    .addEdge(b, c)
    .addEdge(a_, b)
    .addEdge(d, a_)


  "paths" should "return 0 paths for empty graph" in {
    assert(paths(emptyDigraph[Char], a_).isEmpty)
  }

  it should "return 1 path in single edge graph" in {
    val ps: immutable.Seq[List[Char]] = paths(graphSingleEdge, a_)
    assert(ps.size == 1)
    assert(ps.contains(List(b, a_)))
  }

  it should "return 0 paths in a single edge graph if source has 0 out degree" in {
    assert(paths(graphSingleEdge, b).isEmpty)
  }

  it should "return 0 paths in a single edge graph if source is not in the graph" in {
    assert(paths(graphSingleEdge, c).isEmpty)
  }

  it should "return correct paths" in {
    assert(paths(graph2Linear, a_).size == 2)
    assert(paths(graph2Linear, b).size == 1)
    assert(paths(graph4Tree, a_).size == 4)
    assert(paths(graph4Tree, b).size == 2)
    assert(paths(graph4TreeAlt, a_).size == 4)
    assert(paths(graph4TreeAlt, b).size == 2)
    assert(paths(graph4TreeCycled, a_).size == 4)
    assert(paths(graph4TreeCycled, b).size == 3)
  }

  it should "return path with length <= specified length" in {
    val edges = 2
    val vertices = edges + 1
    val ps = paths(graph4TreeCycled, a_)(edges)
    ps.foreach(x => assert(x.size <= vertices))
    assert(ps.size == 3)
  }
}

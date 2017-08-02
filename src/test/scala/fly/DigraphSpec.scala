package fly

class DigraphSpec extends UnitSpec {

  implicit class DSL[T](v: T) {
    def ~>(w: T): Digraph[T] = EmptyDigraph().addEdge(v, w)
  }
  case class GraphDSL[T](v: T, g: Digraph[T]) {
    def ~>(w: T): Digraph[T] = g.addEdge(v, w)
  }

  private def emptyDigraph[T] = EmptyDigraph[T]()
  private def paths[T](g: Digraph[T], source: T) = pathsTailRecursive(g, source, 1)

  private val graph2Line = emptyDigraph.addEdge(1, 2).addEdge(2, 3)
  private val graph4Tree = emptyDigraph.addEdge(1, 2).addEdge(2, 3).addEdge(3, 4).addEdge(1, 4)
  private val graph4TreeAlt = emptyDigraph.addEdge(1, 4).addEdge(3, 4).addEdge(2, 3).addEdge(1, 2)
  private val graph4TreeCycle = emptyDigraph.addEdge(1, 4).addEdge(3, 4).addEdge(2, 3).addEdge(1, 2).addEdge(4, 1)

  "A graph" should "contain return 2 paths" in {
    assert(paths(graph2Line, 1).size == 3)
    assert(paths(graph2Line, 2).size == 2)
    assert(paths(graph4Tree, 1).size == 5)
    assert(paths(graph4Tree, 2).size == 3)
    assert(paths(graph4TreeAlt, 1).size == 5)
    assert(paths(graph4TreeAlt, 2).size == 3)
    assert(paths(graph4TreeCycle, 1).size == 5)
    assert(paths(graph4TreeCycle, 2).size == 4)
  }
}

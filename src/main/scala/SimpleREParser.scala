import scala.collection.mutable


object SimpleREParser {
  val EPS : Char = '#'
  type Node = Int
  type Nodes = mutable.HashSet[Int]
  def main(args: Array[String]): Unit = {
    val g = new Graph
    for (i <- 0 to 9) g.add_node()
    g.add_link(0, 'a', 1)
    g.add_link(1, EPS, 2)
    g.add_link(2, EPS, 3)
    g.add_link(2, EPS, 9)
    g.add_link(3, EPS, 4)
    g.add_link(3, EPS, 6)
    g.add_link(4, 'b', 5)
    g.add_link(6, 'c', 7)

    g.add_link(7, EPS, 8)

    g.add_link(5, EPS, 8)
    g.add_link(8, EPS, 3)
    g.add_link(8, EPS, 9)
    println(g)
    print(Graph.subset_constructor(g))
  }


  type SymbolTable = mutable.HashSet[Char]

  class Status (val nodes: Nodes) extends Comparable[Status] with Iterable[Node] {

    override def hashCode(): Node = {
      if (this.nodes.isEmpty) "".##
      else
        nodes.toArray.sorted.map(_.toString).reduce(_ + _).##
    }

    override def equals(obj: Any): Boolean = {
      if (obj == null) false
      else {
        obj match {
          case status: Status =>
            status.hashCode() == this.hashCode()
          case _ => false
        }
      }
    }
    override def compareTo(o: Status) : Int = {
      if (o == null || o.nodes == null) return 1
      else if (this.nodes == null) return -1
      for (n <- o) {
        if (!nodes.contains(n)) -1
      }
      for (n <- nodes) {
        if (!o.nodes.contains(n)) 1
      }
      0
    }
    override def iterator: Iterator[Node] = {
      nodes.iterator
    }
  }

  object Status {
    val empty_status: Status = new Status(mutable.HashSet[Int]())
  }


  class Graph extends Iterable[mutable.HashMap[Char, Nodes]]{
    val char_set : mutable.HashSet[Char] = new mutable.HashSet[Char]()
    var node_num : Int = 0
    var node_array : Array[mutable.HashMap[Char, Nodes]] = new Array[mutable.HashMap[Char, Nodes]](0)
    override def iterator : Iterator[mutable.HashMap[Char, Nodes]] = node_array.iterator
    def add_node() : Unit = {
      val tmp = new Array[mutable.HashMap[Char,Nodes]](node_num + 1)
      var i = 0
      for (n <- node_array) {
        tmp(i) = n
        i += 1
      }
      tmp(node_num) = new mutable.HashMap[Char, Nodes]()
      node_num += 1
      node_array = tmp
    }

    def add_link(n : Node, c : Char, to : Node) : Unit = {
      val set = node_array(n).get(c)
      if (c != EPS)
        char_set += c
      if (set.nonEmpty) {
        node_array(n).put(c, set.get + to)
      } else {
        node_array(n).put(c, mutable.HashSet[Node](to))
      }
    }

    def eps_closure(n : Node , set : mutable.HashSet[Int]) : Status = {
      val set = node_array(n).get(EPS)
      if (set.nonEmpty) {
        var hashSet = set.get
        for (s <- hashSet) {
          hashSet ++= eps_closure(s, hashSet).nodes
        }
        new Status(hashSet)
      }
      else Status.empty_status
    }
    def eps_closure (ns : Status) : Status = {
      val set = new mutable.HashSet[Node]()
      for (n <- ns) {
        set ++= eps_closure(n, ns.nodes)
      }
      if (set.isEmpty) ns
      else new Status(set ++ ns.nodes)
    }

    def delta (node : Node, char: Char) : mutable.HashSet[Node] = {
      node_array(node).getOrElse(char, null)
    }

    override def toString: String = {
      var i = 0
      val sb = new StringBuffer()
      for (map <- node_array) {
        sb.append(s"($i)")
        for ((k,v) <- map) {
          sb.append(s"\t <-[$k]-($v)")
          sb.append("\n")
        }
        sb.append("\n")
        i += 1
      }
      sb.toString
    }

  }
  object Graph {
    def subset_constructor(graph: Graph) : Graph = {
      val g_ret = new Graph
      var queue = mutable.Queue[Status]()
      var set : mutable.HashSet[Status] = new mutable.HashSet[Status]()
      var map : mutable.HashMap[Status, Int] = new mutable.HashMap[Status, Int]()
      val status = new Status(new mutable.HashSet[Node]() + 0)
      var i = 0
      if (!set.contains(status)) {
        set += status
        map.put(status, i)
        queue.enqueue(status)
        g_ret.add_node()
        i += 1
      }
      while(queue.nonEmpty) {
        val status = queue.dequeue()
        val index = map(status)
        val tmp_status : Status = graph.eps_closure(status)
        for (c <- graph.char_set) {
          val tmp_delta_status : mutable.HashSet[Node] =
            tmp_status.nodes
              .map(graph.delta(_, c))
              .filter(_ != null)
              .flatMap(_.toList)
          if (tmp_delta_status.nonEmpty) {
            var node_set = new mutable.HashSet[Node]()
            tmp_delta_status
              .map(graph.eps_closure(_, tmp_delta_status))
              .flatMap(_.nodes.toList)
              .toSet
            node_set ++= tmp_delta_status
            val new_status = new Status(
              node_set
            )
            if (!set.contains(new_status)) {
              set += new_status
              map.put(new_status, i)
              queue.enqueue(new_status)
              g_ret.add_node()
              g_ret.add_link(index, c, i)
              i += 1
            } else {
              val _i = map(new_status)
              g_ret.add_link(index, c, _i)
            }
          }
        }
      }
      g_ret
    }
  }
}

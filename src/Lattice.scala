/*
  Please refer to licensing information in LICENSE.txt
  Author: Vineeth Kashyap
  Email: vineeth@cs.ucsb.edu
  This file defines Lattice and some operations on it.
*/

import scala.collection.mutable

class DirectedLatticeGraph {
  val edges = mutable.Map.empty[String, mutable.Set[String]]
  edges("L") = mutable.Set("L", "H")
  edges("H") = mutable.Set("H")
  val nodes = mutable.Set("L", "H")

  def addNode(n: String) {
    addEdge("L", n)
    addEdge(n, "H")
  }

  def addEdge(a: String, b: String) {
    if (!edges.contains(a)) {
      edges(a) = mutable.Set.empty[String]
      addNode(a)
    }
    if (!edges.contains(b)) {
      edges(b) = mutable.Set.empty[String]
      addNode(b)
    }
    edges(a) += b
  }

  def isConnected(a: String, b: String): Boolean = if (a == b) true else connected(a, b, mutable.Set.empty[String]) //optimize this by using transitive closure?

  private def connected(a: String, b: String, visited: mutable.Set[String]): Boolean = { //if a is connected to b then a <= b
    if (visited.contains(a)) false
    else if (!edges.contains(a)) false
    else if(edges(a).contains(b)) true
    else {
      visited += a
      edges(a).exists(connected(_, b, visited))
    }
  }

  def isConsistent: Boolean = !(isConnected("H", "L")) //TODO: make this more robust, and work for a more complicated Lattice

}

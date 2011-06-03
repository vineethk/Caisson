/*
  Please refer to licensing information in LICENSE.txt
  Author: Vineeth Kashyap
  Email: vineeth@cs.ucsb.edu
  This file has implementations of Types for Caisson.
*/

sealed abstract class CaissonType {
  def level: Any
}

case class SimpleType(lvl: String) extends CaissonType {
  def level: String = lvl
}

case class CommandType(lvl: SimpleType) extends CaissonType {
  def level: SimpleType = lvl
}

case class StateType(lvl: SimpleType, ptl: List[SimpleType], cnstnts: List[(SimpleType, SimpleType)]) extends CaissonType {
  def level: SimpleType = lvl
  def paramTypeList: List[SimpleType] = ptl
  def constraints: List[(SimpleType, SimpleType)] = cnstnts
}

class CaissonTypeException(msg: String) extends Exception {
  def message: String = msg
}

object TypeUtil {
  def meet(kappa: DirectedLatticeGraph, left: SimpleType, right: SimpleType): SimpleType = { //implement this for a general lattice
    if (kappa.isConnected(left.level, right.level)) left
    else if (kappa.isConnected(right.level, left.level)) right
    else throw new CaissonTypeException("Incompatible types "+left.level+" and "+right.level)
  }

  def join(kappa: DirectedLatticeGraph, left: SimpleType, right: SimpleType): SimpleType = {
    if (kappa.isConnected(left.level, right.level)) right
    else if (kappa.isConnected(right.level, left.level)) left
    else throw new CaissonTypeException("Incompatible types "+left.level+" and "+right.level)
  }
}

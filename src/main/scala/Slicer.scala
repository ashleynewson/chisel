/*
  Copyright Ashley Newson 2016
*/

package Chisel

import collection.mutable.Set

trait Slicer {
  object Criterion {
    object Direction extends Enumeration {
      val None, Forward, Backward = Value
    }
  }

  class Criterion(top: Module, val specification: String) {
    import Criterion._
    /** The source line sequence that specifies the node */
    val (direction, positions) = parseSpecification(specification)
    /** The node this criterion points at */
    val nodes = findNode(top, positions);

    object Position {
      def apply(positionStr: String): Position = {
        val parts = positionStr.split(":")
        parts.size match {
          case 2 => new Position(parts(0), parts(1).toInt)
          case 3 => new Position(parts(0), parts(1).toInt, Some(parts(2).toInt))
          case _ => {ChiselError.error("Invalid source position: " + positionStr); null}
        }
      }
    }
    class Position(val filename: String, val line: Int, val address: Option[Int] = None) {
      def ==(that: StackTraceElement): Boolean = {
        that != null && (filename == that.getFileName() && line == that.getLineNumber())
      }
    }

    /**
     Parses a --slice argument string
     @example "backward@TopModule.scala:15/SubModule1.scala:23"
     */
    def parseSpecification(specification: String): (Direction.Value, List[Position]) = {
      val parts = specification.split("@")
      val direction = parts(0) match {
        case "forward" => Direction.Forward
        case "backward" => Direction.Backward
        case _ => {
          ChiselError.error("Invalid direction on slice criteria \"%s\"".format(specification))
          Direction.None
        }
      }
      val positionStrs = parts(1).split("/")
      val positions = positionStrs.map((str) => Position(str)).toList
      (direction, positions)
    }

    def findNode(top: Module, positions: List[Position]): Set[Node] = {
      val position :: nextPositions = positions
      val found = Set[Node]()
      if (nextPositions == Nil) {
        // Were looking for a node.
        for (node <- top.nodes) {
          if (position == node.line) {
            found += node
          }
        }
      } else {
        // Were looking for a module instantiation.
        for (subModule <- top.children) {
          if (position == subModule.instantiationLine) {
            found ++= findNode(subModule, nextPositions)
          }
        }
      }
      found
    }
  }
}

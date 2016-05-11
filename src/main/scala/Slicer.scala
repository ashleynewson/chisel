/*
  Copyright Ashley Newson 2016
*/

package Chisel

import collection.mutable.Set

trait Slicer {
  val chiselMainLine = {
    val trace = new Throwable().getStackTrace
    ChiselError.findFirstUserLine(trace) getOrElse trace(0)
  }

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
          case 2 => new Position(parts(0), parts(1))
          case 3 => new Position(parts(0), parts(1), Some(parts(2)))
          case _ => {ChiselError.error("Invalid source position: " + positionStr); null}
        }
      }
    }
    class Position(val filename: String, lineAndName: String, bitsString: Option[String] = None) {
      val bits: List[Int] = {
        bitsString match {
          case None => Nil
          case Some(s) => s.split(":").map(_.toInt).toList
        }
      }
      val (name: String, line: Int) = {
        if (lineAndName.indexOf("_") == -1)
          if (lineAndName forall Character.isDigit)
             (null, lineAndName.toInt)
          else
            (lineAndName, 0)
        else
          if (lineAndName.substring(0, lineAndName.indexOf("_")) forall Character.isDigit)
             (lineAndName.substring(lineAndName.indexOf("_") + 1), lineAndName.substring(0, lineAndName.indexOf("_")).toInt)
          else
            (lineAndName, 0)            
      }
      // def ==(that: StackTraceElement): Boolean = {
      //   that != null && (filename == that.getFileName() && line == that.getLineNumber())
      // }
      def matches(that: StackTraceElement, thatName: String, acceptNoName: Boolean): Boolean = {
        that != null &&
        (filename == that.getFileName() || filename == "") &&
        (line == that.getLineNumber() || line == 0) &&
        (if (thatName != null) {
          if (name != null) {
            name == thatName
          } else {
            false
          }
        } else {
          acceptNoName
        })
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
          // if (position == node.line) {
          if (position.matches(node.line, node.name, !(Driver.enableHidding && node.hidden))) {
            found += node
          }
        }
      } else {
        // Were looking for a module instantiation.
        for (subModule <- top.children) {
          if (position.matches(subModule.instantiationLine, subModule.name, true)) {
            found ++= findNode(subModule, nextPositions)
          }
        }
      }
      found
    }
  }
}

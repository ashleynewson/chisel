/*
  Copyright Ashley Newson 2016
*/

package Chisel

import collection.mutable.{Map,Set}

abstract class SimulationNode(val node: Node) extends SimulationAnnotation {
  /** Evaluation begins/ends at clocked nodes. */
  val width = node.width
  val clocked: Boolean
  var inputs: Array[SimulationNode] = null
  protected var outputBits = new SimulationBits(node.width, this)
  /** A node with a higher index is run after a node with a lower index. */
  protected var evaluationIndex = -1
  var clockSet = Set[SimulationClock]()
  var clockSetReady = false
  var forwardTracked = false

  // It may be useful to make groups evaluate together to aid memory cache hits...
  // Not prematurely optimising for now.

  /** The node's output to other nodes */
  def output: SimulationBits = {
    outputBits
  }

  /** Stuff run after linking up the simulation nodes */
  def postLinkSetup(simulation: Simulation): Unit = {
  }

  /** Obtain the list of all SimulationBits associated with this node */
  def getSimulationBits(): Set[SimulationBit] = {
    val bits = Set[SimulationBit]()
    bits ++= outputBits.bits.toSet
    bits
  }

  /**
   * Compute the value of this node's output.
   * The order of evaluation must obey combinatorial flow.
   */
  def evaluate(): Unit

  /**
   * (Compute and) return the ordering of evaluation for this node relative to
   * others, based on the ordering of its inputs.
   */
  def order: Int = {
    if (evaluationIndex == -1) {
      if (clocked) {
        evaluationIndex = 0
      } else {
        for (input <- inputs) {
          val inputOrder = input.order
          if (input.order > evaluationIndex) {
            evaluationIndex = inputOrder
          }
        }
        // It happens after the inputs - not with.
        evaluationIndex += 1
      }
    }
    evaluationIndex
  }

  def addClock(clock: SimulationClock): Unit = {
    clockSet += clock
  }

  /**
   * (Compute and) return the set of clocks which eventually trigger an update.
   */
  def clocks: Set[SimulationClock] = {
    if (clockSetReady == false) {
      if (clocked) {
        // This shouldn't be reached really, but oh well.
      } else {
        for (input <- inputs) {
          clockSet ++= input.clocks
        }
      }
      clockSetReady = true
    }
    clockSet
  }

  def <(that: SimulationNode): Boolean = {
    order < that.order
  }

  // def trackForward(): Unit = {
  //   for (bit <- outputBits.bits) {
  //     bit.trackForward()
  //   }
  // }

  // def isTracked: Boolean = {
  //   outputBits.critical
  // }

  // def traceNodes(): Set[Node] = {
  //   val traceSet = Set[SimulationNode](this)
  //   for (bit <- outputBits.bits) {
  //     if (bit.critical) {
  //       traceSet ++= bit.traceSimulationNodes()
  //     }
  //   }
  //   traceSet.map(_.node)
  // }

  // /** True if affector is a critical input. */
  // def affectedBy(affector: SimulationNode): Boolean = {
  //   return outputBits.criticalInputs.contains(affector)
  // }

  override def dumpJSON(sliceBits: Set[SimulationBit]): String = {
    val builder = new StringBuilder()
    builder.append("{")
    builder.append("\"name\":\"" + annotationName + "\",")
    builder.append("\"type\":\"data\",")
    builder.append("\"width\":" + outputBits.width + ",")
    builder.append("\"size\":1,")

    {
      builder.append("\"state\":\"")
      appendBase64FromBits(builder, outputBits.width, 1, (word: Int, bit: Int) => {outputBits(bit)})
      builder.append("\",")
    }
    {
      builder.append("\"mask\":\"")
      appendBase64FromBits(builder, outputBits.width, 1, (word: Int, bit: Int) => {sliceBits.contains(outputBits(bit))})
      builder.append("\",")
    }

    builder.append("}")
    builder.toString
  }
}

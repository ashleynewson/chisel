/*
  Copyright Ashley Newson 2016
*/

package Chisel

import scala.collection.mutable.Set

class SimulationInput(node: Data) extends SimulationNode(node) {
  val stubbed = (node.inputs.size == 0)
  override val clocked = stubbed

  override def postLinkSetup(): Unit = {
    // This is so that inputs can be modified before every clock tick.
    // It may be more appropriate to use only the implicit clock?
    // Need to refer to specification.
    for (simulationClock <- simulation.simulationClocks) {
      addClock(simulationClock)
    }
  }

  override def evaluate(): Unit = {
    if (stubbed) {
      if (Driver.simulationTest == null) {
        System.err.println("Input for " + node.line.getFileName() + ":" + node.line.getLineNumber())
        val value = readLine()
        if (value != null && value != "") {
          outputBits.long = value.toLong
        } else {
          System.err.println("Input preserved.")
        }
      }
    } else {
      outputBits := inputs(0).output
    }
    // System.err.println("Set " + node.line.getFileName() + ":" + node.line.getLineNumber() + " to " + output.toString())
  }

  override def staticDependencies(bit: SimulationBit): Set[SimulationBit] = {
    if (inputs.size > 0) {
      staticDependencyBit(inputs(0), bit)
    } else {
      Set()
    }
  }

  override def staticDependents(bit: SimulationBit): Set[SimulationBit] = {
    staticDependentBit(inputs(0), bit)
  }
}

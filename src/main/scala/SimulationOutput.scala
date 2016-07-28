/*
  Copyright Ashley Newson 2016
*/

package Chisel

import scala.collection.mutable.Set

class SimulationOutput(node: Data) extends SimulationNode(node) {
  override val clocked = false

  override def evaluate(): Unit = {
    outputBits := inputs(0).output
  }

  override def staticDependencies(bit: SimulationBit): Set[SimulationBit] = {
    staticDependencyBit(inputs(0), bit)
  }

  override def staticDependents(bit: SimulationBit): Set[SimulationBit] = {
    staticDependentBit(inputs(0), bit)
  }
}
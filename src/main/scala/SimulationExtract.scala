/*
  Copyright Ashley Newson 2016
*/

package Chisel

import scala.collection.mutable.Set

class SimulationExtract(node: Extract) extends SimulationNode(node) {
  override val clocked = false

  // Need to check dependence validity
  override def evaluate(): Unit = {
    // Can the lo and hi be non constant?
    val hi = inputs(1).output.int
    val lo = inputs(2).output.int
    for (i <- lo to hi) {
      outputBits(i - lo) := inputs(0).output(i)
    }
    outputBits.depend(inputs(1).output)
    outputBits.depend(inputs(2).output)
  }

  override def staticDependencies(bit: SimulationBit): Set[SimulationBit] = {
    val dependencies = Set[SimulationBit]()
    val hi = inputs(1).output.int
    val lo = inputs(2).output.int
    for (i <- lo to hi) {
      if (outputBits(i - lo) == bit) {
        dependencies += inputs(0).output(i)
      }
    }
    dependencies ++= inputs(1).output.bits
    dependencies ++= inputs(2).output.bits
    dependencies
  }

  override def staticDependents(bit: SimulationBit): Set[SimulationBit] = {
    val dependents = Set[SimulationBit]()
    val hi = inputs(1).output.int
    val lo = inputs(2).output.int
    for (i <- lo to hi) {
      if (inputs(0).output(i) == bit) {
        dependents += outputBits(i - lo)
      }
    }
    dependents
  }
}

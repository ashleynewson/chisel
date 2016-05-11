/*
  Copyright Ashley Newson 2016
*/

package Chisel

import scala.collection.mutable.Set

class SimulationReg(node: Reg) extends SimulationNode(node) {
  override val clocked = true

  // System.err.println("Reg with " + node.inputs.size + " inputs @" + node.line.getLineNumber())
  // val resetVal: BigInt = if (node.inputs.size == 2) node.inputs(1).asInstanceOf[Literal].value else 0
  // System.err.println(resetVal)

  // outputBits.bigInt = resetVal

  /** Stuff run after linking up the simulation nodes */
  override def postLinkSetup(): Unit = {
    if (!simulation.staticOnly) {
      doReset()
    }
  }

  def doReset(): Unit = {
    if (inputs.size == 2) {
      outputBits := inputs(1).output
    } else {
      outputBits.bigInt = 0
    }
  }

  override def evaluate(): Unit = {
    if (simulation.reset) {
      doReset()
      // outputBits.clear()
      // outputBits.bigInt = resetVal
    } else {
      outputBits := inputs(0).output
    }
    // System.err.println("Reg " + node.line.getFileName() + ":" + node.line.getLineNumber() + " to " + output.toString())
  }

  override def staticDependencies(bit: SimulationBit): Set[SimulationBit] = {
    staticDependencyBit(inputs(0), bit)
  }

  override def staticDependents(bit: SimulationBit): Set[SimulationBit] = {
    staticDependentBit(inputs(0), bit)
  }
}

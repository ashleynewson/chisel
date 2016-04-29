/*
  Copyright Ashley Newson 2016
*/

package Chisel

/** Used only as a way of tapping Data nodes */
class SimulationLiteral(node: Literal) extends SimulationNode(node) {
  override val clocked = false
  outputBits.long = node.value.toLong

  override def postLinkSetup(simulation: Simulation): Unit = {
    if (inputs.size > 0) {
      var baseLit = this
      while (baseLit.inputs.size > 0) {
        baseLit = baseLit.inputs(0).asInstanceOf[SimulationLiteral]
      }
      outputBits := baseLit.output
    }
  }

  override def clearDependencies(): Unit = {
    // Don't clear anything!
  }

  override def evaluate(): Unit = {
  }
}

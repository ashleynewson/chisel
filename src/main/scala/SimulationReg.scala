/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationReg(node: Reg) extends SimulationNode(node) {
  override val clocked = true

  override def evaluate(): Unit = {
    outputBits := inputs(0).output
  }
}

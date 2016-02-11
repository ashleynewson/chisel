/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationMemWrite(node: MemWrite) extends SimulationNode(node) {
  override val clocked = true

  override def evaluate(): Unit = {
  }
}

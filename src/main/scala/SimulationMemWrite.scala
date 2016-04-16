/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationMemWrite(node: MemWrite) extends SimulationNode(node) {
  override val clocked = false

  override def evaluate(): Unit = {
    // Implementation in SimulationMem
    // What if multiple clock domains?
  }
}

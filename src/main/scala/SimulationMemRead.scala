/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationMemRead(node: MemRead) extends SimulationNode(node) {
  override val clocked = false
  var mem: SimulationMem = null

  override def postLinkSetup(simulation: Simulation): Unit = {
    mem = inputs(1).asInstanceOf[SimulationMem]
  }

  override def evaluate(): Unit = {
    outputBits.clear()
    outputBits := mem.read(inputs(0).output.int)
    outputBits.depend(inputs(0).output)
    outputBits.depend(mem.output)
  }
}

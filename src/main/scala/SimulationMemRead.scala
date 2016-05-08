/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationMemRead(node: MemRead) extends SimulationNode(node) {
  override val clocked = false
  var mem: SimulationMem = null
  var invalid: SimulationBits = null

  override def postLinkSetup(): Unit = {
    mem = inputs(1).asInstanceOf[SimulationMem]
    invalid = new SimulationBits(mem.node.width)
    invalid.clear()
  }

  override def evaluate(): Unit = {
    outputBits.clear()
    if (inputs(0).output.int < mem.size) {
      outputBits := mem.read(inputs(0).output.int)
    } else {
      outputBits := invalid
    }
    outputBits.depend(inputs(0).output)
    outputBits.depend(mem.output)
  }
}

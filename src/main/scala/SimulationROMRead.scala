/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationROMRead(node: ROMRead) extends SimulationNode(node) {
  override val clocked = false
  var rom: SimulationROMData = null
  var invalid: SimulationBits = null

  override def postLinkSetup(simulation: Simulation): Unit = {
    rom = inputs(1).asInstanceOf[SimulationROMData]
    invalid = new SimulationBits(rom.node.width)
    invalid.clear()
  }

  override def evaluate(): Unit = {
    outputBits.clear()
    if (inputs(0).output.int < rom.size) {
      outputBits := rom.read(inputs(0).output.int)
    } else {
      outputBits := invalid
    }
    outputBits.depend(inputs(0).output)
    outputBits.depend(rom.output)
  }
}

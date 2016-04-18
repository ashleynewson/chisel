/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationReg(node: Reg) extends SimulationNode(node) {
  override val clocked = true
  var simulation: Simulation = null

  // System.err.println("Reg with " + node.inputs.size + " inputs @" + node.line.getLineNumber())
  val resetVal: BigInt = if (node.inputs.size == 2) node.inputs(1).asInstanceOf[Literal].value else 0
  // System.err.println(resetVal)

  outputBits.bigInt = resetVal

  /** Stuff run after linking up the simulation nodes */
  override def postLinkSetup(s: Simulation): Unit = {
    simulation = s
  }

  override def evaluate(): Unit = {
    if (simulation.reset) {
      outputBits.clear()
      outputBits.bigInt = resetVal
    } else {
      outputBits := inputs(0).output
    }
    // System.err.println("Reg " + node.line.getFileName() + ":" + node.line.getLineNumber() + " to " + output.toString())
  }

  override def annotationName: String = {"Reg " + inputsString}
}

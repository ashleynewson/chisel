/*
  Copyright Ashley Newson 2016
*/

package Chisel

class SimulationMem(node: Mem[_]) extends SimulationNode(node) {
  override val clocked = true

  val data = new Array[SimulationBits](node.n)
  for (i <- 0 to node.n-1) {
    data(i) = new SimulationBits(node.width)
  }

  override def evaluate(): Unit = {
    for (input <- inputs) {
      val writer = input.asInstanceOf[SimulationMemWrite]

      if (writer.inputs(1).output(0)) {
        write(writer.inputs(0).output.int, writer.inputs(2).output)
      }
    }
  }

  def write(addr: Int, datum: SimulationBits): Unit = {
    data(addr) := datum
  }

  def read(addr: Int): SimulationBits = {
    data(addr)
  }
}

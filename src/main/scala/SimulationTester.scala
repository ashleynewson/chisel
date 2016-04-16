/*
  Copyright Ashley Newson 2016
*/

package Chisel

import Chisel._

// Doesn't work the same way as a normal tester.
class SimulationTester(module: Module, simulation: Simulation) {

  def peek(data: Bits): BigInt = {
    simulation.getSimulationNode(data).output.bigInt
  }
  def peek(data: Flo): Float = {
    simulation.getSimulationNode(data).output.float
  }
  def peek(data: Dbl): Double = {
    simulation.getSimulationNode(data).output.double
  }
  def peekAt[T <: Bits](data: Mem[T], addr: Int): BigInt = {
    simulation.getSimulationNode(data).asInstanceOf[SimulationMem].read(addr).bigInt
  }

  def poke(data: Bits, x: Boolean): Unit = {
    simulation.getSimulationNode(data).output.int != 0
  }
  def poke(data: Bits, x: Int): Unit = {
    simulation.getSimulationNode(data).output.int = x
  }
  def poke(data: Bits, x: Long): Unit = {
    simulation.getSimulationNode(data).output.long = x
  }
  def poke(data: Bits, x: BigInt): Unit = {
    simulation.getSimulationNode(data).output.bigInt = x
  }
  def poke(data: Bits, x: Float): Unit = {
    simulation.getSimulationNode(data).output.float = x
  }
  def pokeAt[T <: Bits](data: Mem[T], value: BigInt, addr: Int): BigInt = {
    // Yes, read.
    simulation.getSimulationNode(data).asInstanceOf[SimulationMem].read(addr).bigInt = value
  }

  def expect(good: Boolean, msg: => String): Boolean = {
    // Stub
    true
  }

  def step(n: Int): Unit = {
    for (i <- 1 to n) {
      simulation.step()
    }
  }
}

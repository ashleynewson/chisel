/*
  Copyright Ashley Newson 2016
*/

package Chisel

object SimulationTester {
  object DependenceSet {
    val activeSets = scala.collection.mutable.Set[DependenceSet]()

    def apply(bitses: SimulationBits*): DependenceSet = {
      val set = new DependenceSet()
      for (bits <- bitses) {
        set.depend(bits, (BigInt(2) << (bits.width)) - 1)
      }
      set
    }

    def freeze_traces(): Unit = {
      for (set <- activeSets) {
        set.set.preserve()
      }
    }
    def unfreeze_traces(): Unit = {
      for (set <- activeSets) {
        set.set.unpreserve()
      }
    }
  }

  class DependenceSet {
    val set = new AccumulatorSet[SimulationBit]()
    // set.preserve()
    DependenceSet.activeSets += this

    def depend(data: SimulationBits): DependenceSet = {
      depend(data, (BigInt(1) << (data.width) - 1))
    }
    def depend(data: SimulationBits, mask: BigInt): DependenceSet = {
      var i = 0;
      var submask = BigInt(1)
      // set.unpreserve()
      for (i <- 0 until data.width) {
        if ((submask & mask) != 0) {
          set ++= data(i).criticalInputs
          set += data(i)
        }
        submask <<= 1
      }
      // set.preserve()
      this
    }
    def depend(that: DependenceSet): DependenceSet = {
      // set.unpreserve()
      set ++= that.set
      // set.preserve()
      this
    }

    def disuse(): Unit = {
      // set.unpreserve()
      set.freeze()
      set.collapse()
      DependenceSet.activeSets -= this
    }
  }
}

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

  // For use with dependence tracking. Should be done on IOs.
  def bitsOf(data: Bits): SimulationBits = {
    simulation.getSimulationNode(data).output
  }
  // def bitsAt[T <: Bits](data: Mem[T], addr: Int): SimulationBits = {
  //   simulation.getSimulationNode(data).asInstanceOf[SimulationMem].read(addr)
  // }

  def poke(data: Bits, x: Boolean): Unit = {
    simulation.getSimulationNode(data).output(0) = x
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

  def resetSlice(): Unit = {
    simulation.clearDependencies()
  }

  def startSlice(): Unit = {
    simulation.enableSlicing()
  }

  def stopSlice(): Unit = {
    simulation.disableSlicing()
  }
}

/*
 Copyright Ashley Newson 2016
 */

package Chisel

import collection.mutable.{Set}

class SimulationMem(node: Mem[_]) extends SimulationNode(node) {
  override val clocked = true
  val size = node.n

  /* Used to cheaply track dependencies for nodes without proper access */
  val unwritten = new SimulationBits(1, this)
  val unwrittenAll = new SimulationBits(1, this)
  val tainted = Set[SimulationBits]() // Words
  val taintedBits = Set[SimulationBit]()

  var data: Array[SimulationBits] = null

  override def postLinkSetup(): Unit = {
    if (simulation.staticOnly) {
      data = new Array[SimulationBits](0)
    } else {
      data = new Array[SimulationBits](node.n)
      for (i <- 0 to node.n-1) {
        data(i) = new SimulationBits(node.width, this)
      }
    }
  }

  override def getSimulationBits(): Set[SimulationBit] = {
    val bits = Set[SimulationBit]()
    bits ++= output.bits.toSet
    bits ++= unwritten.bits.toSet
    bits ++= unwrittenAll.bits.toSet
    for (datum <- data) {
      bits ++= datum.bits.toSet
    }
    bits
  }

  override def clearDependencies(): Unit = {
    tainted.clear()
    taintedBits.clear()
    super.clearDependencies()
  }

  // Need to revise the technicallity vs utility of this section.
  override def evaluate(): Unit = {
    if (simulation.reset) {
      // Don't make any modifications whilst preping simulation.
      return
    }
    for (input <- inputs) {
      val writer = input.asInstanceOf[SimulationMemWrite]

      if (writer.inputs(1).output(0)) {
        val target = data(writer.inputs(0).output.int)
        write(writer.inputs(0).output.int, writer.inputs(2).output)
        // Depend on address
        target.depend(writer.inputs(0).output)
        // Depend on enable
        target.depend(writer.inputs(1).output)
        // Depend on writer
        target.depend(writer.output)

        tainted += target
        taintedBits ++= target.bits
      }
    }
    if (Driver.strictSlice) {
      unwritten.clear()
      for (input <- inputs) {
        val writer = input.asInstanceOf[SimulationMemWrite]

        unwritten.depend(writer.output)
        if (writer.inputs(1).output(0)) {
          unwritten.depend(writer.inputs(0).output)
        } else {
          unwritten.depend(writer.inputs(1).output)
        }
      }

      unwrittenAll(0).checkpoint(false)
      unwrittenAll(0).depend(unwritten(0))
      for (bit <- taintedBits) {
        // Using false represents a better match for a high level 
        // interpretation of the circuit.
        bit.checkpoint(false)
        bit.depend(unwritten(0))
      }
    }
  }

  // Need to revise the technicallity vs utility of this section.
  def write(addr: Int, datum: SimulationBits): Unit = {
    data(addr) := datum
  }

  def read(addr: Int): SimulationBits = {
    if (Driver.strictSlice && !tainted.contains(data(addr))) {
      data(addr).checkpoint(false)
      data(addr).depend(unwrittenAll)
    }
    data(addr)
  }

  override def extra_dependence(bit: SimulationBit): Set[SimulationBit] = {
    if (Driver.strictSlice && !taintedBits.contains(bit)) {
      unwrittenAll(0).criticalInputs
    } else {
      Set()
    }
  }

  override def isInSlice(sliceBits: Set[SimulationBit]): Boolean = {
    if (outputBits.isInSlice(sliceBits)) {
      return true
    } else {
      for (word <- data) {
        if (outputBits.isInSlice(sliceBits)) {
          return true
        }
      }
    }
    return false
  }

  override def dumpJSON(sliceBits: Set[SimulationBit]): String = {
    val builder = new StringBuilder()
    builder.append("{")
    builder.append("\"name\":\"" + node.annotationName + "\",")
    builder.append("\"type\":\"data\",")
    builder.append("\"in\":" + isInSlice(sliceBits) + ",")
    builder.append("\"hide\":" + isHidden + ",")
    builder.append("\"width\":" + node.width + ",")
    builder.append("\"size\":" + node.n + ",")

    {
      builder.append("\"state\":\"")
      appendBase64FromBits(builder, node.width, node.n, (word: Int, bit: Int) => {data(word)(bit)})
      builder.append("\",")
    }
    {
      builder.append("\"mask\":\"")
      appendBase64FromBits(builder, node.width, node.n, (word: Int, bit: Int) => {sliceBits.contains(data(word)(bit))})
      builder.append("\",")
    }

    builder.append("}")
    builder.toString
  }

  override def staticDumpJSON(sliceBits: Set[SimulationBit]): String = {
    val builder = new StringBuilder()
    builder.append("{")
    builder.append("\"name\":\"" + node.annotationName + "\",")
    builder.append("\"type\":\"mask\",")
    builder.append("\"in\":" + isInSlice(sliceBits) + ",")
    builder.append("\"hide\":" + isHidden + ",")
    builder.append("\"width\":" + outputBits.width + ",")
    builder.append("\"size\":1,")
    builder.append("\"actualsize\":" + node.n + ",")

    {
      builder.append("\"mask\":\"")
      appendBase64FromBits(builder, outputBits.width, 1, (word: Int, bit: Int) => {sliceBits.contains(outputBits(bit))})
      builder.append("\",")
    }

    builder.append("}")
    builder.toString
  }
}

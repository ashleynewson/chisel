/*
  Copyright Ashley Newson 2016
*/

package Chisel

import collection.mutable.{Map,Set}

class Simulation(val topModule: Module) {
  var canSlice = true

  private val nodeMap = Map[Node,SimulationNode]()
  var simulationNodes = List[SimulationNode]()

  private val clockMap = Map[Clock,SimulationClock]()
  val simulationClocks = Set[SimulationClock]()
  private var nextClockToTick: SimulationClock = null

  private val simulationBits = Set[SimulationBit]()

  private var cycles = 0

  var reset: Boolean = true

  addSimulationNodes(topModule)
  for (clock <- Driver.clocks) {
    if (nodeMap.get(clock).isEmpty) {
      nodeMap += (clock -> clock.getSimulationNode())
    }
  }
  linkSimulationNodes()
  findSimulationClocks()
  for (simulationNode <- nodeMap.values) {
    simulationNode.postLinkSetup(this)
  }
  associateClocks()
  orderSimulationNodes()
  nextClockToTick = simulationClocks.head
  for (simulationNode <- nodeMap.values) {
    simulationBits ++= simulationNode.getSimulationBits()
  }
  // {
  //   var clocked = 0
  //   nodeMap.values.foreach((x) => (if (x.clocked) (clocked+=1)))
  //   System.err.println("Clocked: " + clocked + "/" + nodeMap.size)
  // }

  private def findSimulationClocks(): Unit = {
    for (simulationNode <- nodeMap.values) {
      simulationNode match {
        case simulationClock: SimulationClock => {
          clockMap += (simulationClock.clock -> simulationClock)
          simulationClocks += simulationClock
        }
        case _ => ()
      }
    }
  }

  def getSimulationBits(): Set[SimulationBit] = {
    simulationBits.clone()
  }

  private def addSimulationNodes(module: Module): Unit = {
    for (node <- module.nodes) {
      nodeMap += (node -> node.getSimulationNode())
    }
    for (subModule <- module.children) {
      addSimulationNodes(subModule)
    }
  }

  private def associateClocks(): Unit = {
    for (simulationNode <- nodeMap.values) {
      val clock = simulationNode.node.clock
      if (clock.isDefined) {
        val simulationClock = clockMap(clock.get)
        simulationNode.addClock(simulationClock)
      }
    }
    for (simulationNode <- nodeMap.values) {
      for (simulationClock <- simulationNode.clocks) {
        simulationClock.addUser(simulationNode)
      }
    }
  }

  private def linkSimulationNodes(): Unit = {
    for ((node, simulationNode) <- nodeMap) {
      simulationNode.inputs = node.inputs.map(n => nodeMap(n)).toArray
    }
  }

  private def orderSimulationNodes(): Unit = {
    simulationNodes = nodeMap.values.toList.sortWith(_ < _)
    for (simulationClock <- simulationClocks) {
      simulationClock.orderSimulationNodes()
    }
  }

  def run(): Unit = {
    while (true) {
      System.err.println("Simulation command?")
      val action = readLine()
      if (action == null || action == "") {
        step()
      } else if (action == "end") {
        return
      }
    }
  }

  def step(): Unit = {
    val timeSkip = nextClockToTick.remaining
    nextClockToTick.step()
    for (simulationClock <- simulationClocks) {
      simulationClock.remaining -= timeSkip
    }
    nextClockToTick.remaining = nextClockToTick.period
    for (simulationClock <- simulationClocks) {
      if (simulationClock.remaining < nextClockToTick.remaining) {
        nextClockToTick = simulationClock
      }
    }
    cycles += 1
    if (cycles % 128 == 0) {
      flattenTraces()
    }
  }

  def clearDependencies(): Unit = {
    for (simulationNode <- simulationNodes) {
      simulationNode.clearDependencies()
    }
  }

  def enableSlicing(): Unit = {
    if (canSlice) {
      Driver.traceSimulation = true
    }
  }

  def disableSlicing(): Unit = {
    Driver.traceSimulation = false
  }

  def flattenTraces(): Unit = {
    if (Driver.traceSimulation) {
      for (bit <- simulationBits) {
        bit.freeze_trace()
      }
      SimulationTester.DependenceSet.freeze_traces()
      AccumulatorSet.flattenAll()
      SimulationTester.DependenceSet.unfreeze_traces()
      for (bit <- simulationBits) {
        bit.unfreeze_trace()
      }
    }
  }

  // def forwardTraceNode(forwardFrom: Node): Set[Node] = {
  //   val fromSimulationNode = nodeMap(forwardFrom)
  //   val traceSet = Set[Node]()
  //   for ((node, simulationNode) <- nodeMap) {
  //     if (simulationNode.affectedBy(fromSimulationNode)) {
  //       traceSet += node
  //     }
  //   }
  //   traceSet
  // }

  // def backwardTraceNode(backwardFrom: Node): Set[Node] = {
  //   val fromSimulationNode = nodeMap(backwardFrom)
  //   val traceSet = Set[Node]()
  //   for ((node, simulationNode) <- nodeMap) {
  //     if (fromSimulationNode.affectedBy(simulationNode)) {
  //       traceSet += node
  //     }
  //   }
  //   traceSet
  // }

  def forwardTraceBit(forwardFrom: SimulationBit): Set[SimulationBit] = {
    val traceSet = Set[SimulationBit]()
    for (simulationBit <- simulationBits) {
      if (simulationBit.affectedBy(forwardFrom)) {
        traceSet += simulationBit
      }
    }
    traceSet
  }

  def backwardTraceBit(backwardFrom: SimulationBit): Set[SimulationBit] = {
    val traceSet = Set[SimulationBit]()
    for (simulationBit <- simulationBits) {
      if (backwardFrom.affectedBy(simulationBit)) {
        traceSet += simulationBit
      }
    }
    traceSet
  }

  def getSliceNodes(sliceBits: Set[SimulationBit]): Set[Node] = {
    val nodeSet = Set[Node]()
    for ((node, simulationNode) <- nodeMap) {
      if (simulationNode.isInSlice(sliceBits)) {
        nodeSet += node
      }
    }
    nodeSet
  }

  def getSimulationNode(node: Node): SimulationNode = {
    nodeMap(node)
  }
}

/*
  Copyright Ashley Newson 2016
*/

package Chisel

import collection.mutable.{Map,Set}

class Simulation(val topModule: Module) {
  private val nodeMap = Map[Node,SimulationNode]()
  var simulationNodes = List[SimulationNode]()

  private val clockMap = Map[Clock,SimulationClock]()
  val simulationClocks = Set[SimulationClock]()
  private var nextClockToTick: SimulationClock = null

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
  }

  def addForwardNode(node: Node): Unit = {
    nodeMap(node).trackForward()
  }

  def traceNodes(): Set[Node] = {
    val traceSet = Set[Node]()
    for ((_, simulationNode) <- nodeMap) {
      if (simulationNode.isTracked) {
        traceSet ++= simulationNode.traceNodes()
      }
    }
    traceSet
  }

  def criticalNodes(): Set[Node] = {
    val traceSet = Set[Node]()
    for ((_, simulationNode) <- nodeMap) {
      if (simulationNode.isTracked) {
        traceSet += simulationNode.node
      }
    }
    traceSet
  }
}

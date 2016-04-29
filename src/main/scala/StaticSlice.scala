/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

package Chisel

import collection.mutable.{Map,Set,SortedSet}

class StaticSliceBackend extends Backend with Slicer {
  import PartitionIslands._
  var islands = Array[Island]()
  val allDottable = false
  val useComponentNames = false
  var criteria: List[Criterion] = Nil
  /** Nodes which are the start of any slice */
  val seedNodes = Set[Node]()
  /**
   Nodes which are formed from the intersection of slices from all
   criteria
   */
  var sliceNodes: Set[Node] = null
  // var sliceLinks: Map[Node,Map[Node,Set[StackTraceElement]]] = null
  /** These source files will be converted into html pages */
  val requiredSourceFiles = Set[String]()

  Driver.getLineNumbers = true
  Driver.refineNodeStructure = false

  object NodeSlice {
    def fromCriterion(criterion: Criterion): NodeSlice = {
      criterion.direction match {
        case Criterion.Direction.Backward => new BackwardNodeSlice(criterion)
        case Criterion.Direction.Forward  => new ForwardNodeSlice(criterion)
      }
    }
  }
  abstract class NodeSlice(val criterion: Criterion) {
    /** Nodes of interest (traced nodes) */
    val nodes = Set[Node]()
    // val links = Set[StackTraceElement]()
    var newNodes = Set[Node]()
    /** Find some more nodes of interest */
    def traceStep(latestNodes: Set[Node]): Set[Node]
    /** Find all nodes of interest */
    def trace(): Unit = {
      newNodes = nodes
      do {
        newNodes = traceStep(newNodes)
        nodes ++= newNodes
      } while (newNodes.size > 0)
      // for (node <- nodes) {
      //   val trackingInputs = node.inputs.asInstanceOf[Node.TrackingArrayBuffer[Node]]
      //   for ((consumer, lineSet) <- trackingInputs.lineMap) {
      //     if (nodes(consumer)) {
      //       if (links.contains(node)) {
      //         links(node) += (consumer -> lineSet)
      //       } else {
      //         links(node) = Map((consumer -> lineSet))
      //       }
      //     }
      //   }
      // }
    }
  }
  class BackwardNodeSlice(criterion: Criterion) extends NodeSlice(criterion) {
    override def traceStep(latestNodes: Set[Node]): Set[Node] = {
      val newNodes = Set[Node]()
      for (recentNode <- latestNodes) {
        newNodes ++= recentNode.inputs
      }
      // Don't cause needless rescanning
      newNodes -- nodes
    }

    nodes ++= criterion.nodes
    trace()
  }
  class ForwardNodeSlice(criterion: Criterion) extends NodeSlice(criterion) {
    override def traceStep(latestNodes: Set[Node]): Set[Node] = {
      val newNodes = Set[Node]()
      for (recentNode <- latestNodes) {
        newNodes ++= recentNode.consumers
      }
      // Don't cause needless rescanning
      newNodes -- nodes
    }

    nodes ++= criterion.nodes
    trace()
  }

  override def emitRef(node: Node): String = {
    node match {
      // If we're explicitly outputting literal nodes, make sure they have a legitimate name and not just a number.
      case l: Literal => "L" + l.toString
      // case l: Literal if (allDottable) => "L" + l.toString
      case r: Reg =>
        if (r.name == "") r.name = "R" + r.emitIndex
        fullyQualifiedName(node)
      case _ =>
        if(node.name == "") node.name = "T" + node.emitIndex
        fullyQualifiedName(node)
    }
  }

  private def isDottable (m: Node): Boolean = {
    if (allDottable) {
      true
    } else
    if (m.component.resetPin != None && m == m.component.resetPin.get) {
      false
    } else {
      m match {
        // case x: Literal  => false;
        case _           => true;
      }
    }
  }


  private def asValidLabel( node: Node ): String = {
    val res =
    node match {
      case operator: Op => if (operator.op == "") "?" else operator.op;
      case _             => {
        val typeName = node.getClass.getName.substring(7)
        node.name + ":" + typeName
      }
    }
    if (useComponentNames) {
      node.component.name + "/" + res
    } else {
      res
    }
  }

  private def isNodeInIsland(node: Node, island: Island): Boolean = {
    return island == null || island.nodes.contains(node)
  }

  // private def isLinkInSlice(from: Node, to: Node): Boolean = {
  //   sliceNodes(from) && sliceNodes(to)
  // }

  // private def linkTraces(from: Node, to: Node): Set[StackTraceElement] = {
  //   if (from.inputs.asInstanceOf[Node.TrackingArrayBuffer[Node]].lineMap.contains(to)) {
  //     from.inputs.asInstanceOf[Node.TrackingArrayBuffer[Node]].lineMap(to)
  //   } else {
  //     return Set()
  //   }
  // }

  private def emitModuleText(top: Module, depth: Int, basedir: String): (String, String, String, Boolean) = {
    val res = new StringBuilder()
    val crossings = new StringBuilder()
    val indent = "  " * (depth + 1)

    // The sub-modules which need to be linked to
    val sliceModules = SortedSet[Int]()
    // Every line of source which is in the slice
    val sliceLines = SortedSet[Int]()
    val childrenJson = Set[String]()
    val annotationsJson = Set[String]()
    var inSlice = false

    for (child <- top.children) {
      if (! Driver.partitionIslands) {
        /* Prefix by "cluster" for graphviz to draw a bounding box. */
        res.append(indent)
        res.append("subgraph cluster" + emitRef(child) + "{\n")
        res.append("  " + indent)
        // res.append("label = \"" + child.name + "\"\n")
        res.append("label = \"" + child.name + "\n@" + child.instantiationLine + "\"\n")
      }
      val (innertext, innercrossings, childSliceJson, childInSlice) = emitModuleText(child, depth + 1, basedir + "/" + child.instantiationLine.getLineNumber() + "_" + child.name)
      res.append(innertext)
      if (! Driver.partitionIslands) {
        res.append(indent)
//        res.append("// end " + child.name + "\n")
        res.append("}\n")
        res.append(indent)
      }
      res.append(innercrossings)

      if (childSliceJson != null) {
        if (chiselMainLine.equals(child.instantiationLine)) {
          childrenJson += "\"%s_%s\":%s".format("?", child.name, childSliceJson)
        } else {
          sliceLines += child.instantiationLine.getLineNumber()
          childrenJson += "\"%s_%s\":%s".format(child.instantiationLine.getLineNumber(), child.name, childSliceJson)
        }
        if (childInSlice) {
          inSlice = true
        }
      }
    }

    var EOL = "\n"
    def outputAnIsland(island: Island) {
      val island_res = new StringBuilder()
      val islandId = if (island == null) 0 else island.islandId
      // Should we identify ports? Currently, only for Mux nodes.
      def usePort(n: Node) = n match {
        case mux: Mux => true
        case _ => false
      }

      for (m <- top.nodes) {
        val lineStr = (if (chiselMainLine.equals(m.line)) "?" else m.line.getLineNumber())
        annotationsJson += "\"%s_%s\":%s".format(lineStr, m.name, "{\"name\":\"" + m.annotationName + "\",\"type\":\"bool\",\"in\":" + (if (sliceNodes.contains(m)) 1 else 0) + ",\"hide\":" + m.hidden + "}")
      }

      for (m <- top.nodes) {
        if (isDottable(m)) {
          if( m.component == top ) {
            /* We have to check the node's component agrees because output
             nodes are part of a component *mods* as well as its parent *mods*! */
            if (isNodeInIsland(m, island)) {
              island_res.append(indent)
              island_res.append(emitRef(m));
            }
            var label  = "label=\"" + asValidLabel(m) + "\n@" + m.line
            val anyLit = m.inputs.find(x => !isDottable(x));
            if (!anyLit.isEmpty) {
              var i = 0;
              label += "(";
              for (in <- m.inputs) {
                if (i != 0) label += ", ";
                label += (if (in.isLit) emitRef(in) else "_");
                i += 1;
              }
              label += ")";
            }
            label += "\""
            var color = "color=\"black\""
            // if (seedNodes(m)) {
            //   color = "color=\"red\""
            // } else 
            if (sliceNodes(m)) {
              color = "color=\"green\""
            }

            if (isNodeInIsland(m, island)) {
              m match {
                case reg: Delay => island_res.append("[shape=square," + label + "," + color + "];" + EOL)
                case mux: Mux => island_res.append("[shape=Mdiamond," + label + "," + color + "];" + EOL)
                case _ => island_res.append("[" + label + "," + color + "];" + EOL)
              }
            }

            if (sliceNodes(m)) {
              if (!chiselMainLine.equals(m.line)) {
                sliceLines += m.line.getLineNumber()
              }
              inSlice = true
            }
          }
        }
      }
      for (m <- top.nodes) {
        if( m.component == top && isDottable(m)) {
          /* We have to check the node's component agrees because output
           nodes are part of a component *mods* as well as its parent *mods*! */
          val muxPort = usePort(m)
          for ((in, index) <- m.inputs.zip(List("n", "w", "e"))) {
            if (isDottable(in)) {
              if (isNodeInIsland(in, island)) {
                val srcPort = if (usePort(in)) {
                  ":s"
                } else {
                  ""
                }
                val dstPort = if (muxPort) {
                  ":" + index
                } else {
                  ""
                }
                var color = "color=\"black\""
                // if (isLinkInSlice(in, m)) {
                //   color = "color=\"green\""
                // }
                val edge = (emitRef(in) + srcPort  + " -> " + emitRef(m) + dstPort
                  + "[label=\"" + in.needWidth() + "\"," + color + "];"+ EOL)
                if (islandId != 0) {
                  // If we're drawing partitioned islands, duplicate the logic
                  // for boundary crossings below.
                  if (! (isNodeInIsland(in, island) && (isNodeInIsland(m, island)))) {
                    crossings.append(edge)
                  } else {
                    island_res.append(indent)
                    island_res.append(edge);
                  }
                } else
                /* If the both ends of an edge are on either side of a component
                 boundary, we must add it at the upper level otherwise graphviz
                 will incorrectly draw the input node into the cluster. */
                if( in.component != top && !top.children.contains(in.component) ) {
                  crossings.append(edge)
                } else {
                  island_res.append(indent)
                  island_res.append(edge);
                }
                // if (isLinkInSlice(in, m)) {
                //   sliceLines ++= linkTraces(in, m).map(a => a.getLineNumber())
                // }
              }
            }
          }
        }
      }

      if (island_res.length > 0) {
        if (islandId != 0) {
          res.append("subgraph clusterIsland_" + islandId + " {\n")
        }

        res.append(island_res)

        if (islandId != 0) {
          res.append("label = \"Island_" + islandId + "\";\n")
          res.append("}\n")
        }
      }
    }

    if (islands.isEmpty) {
        outputAnIsland(null)
    } else {
      for (island <- islands) {
        outputAnIsland(island)
      }
    }

    var sliceJson: String = null
    // if (sliceLines.size > 0) {
      requiredSourceFiles += top.constructorLine.getFileName

      val sliceJsonBuilder = new StringBuilder()
      sliceJsonBuilder.append("{")
      sliceJsonBuilder.append("\"in\":" + inSlice + ",")
      sliceJsonBuilder.append("\"file\":\"" + top.constructorLine.getFileName + "\",")
      // sliceJsonBuilder.append("\"lines\":[" + sliceLines.mkString(",") + "],")
      sliceJsonBuilder.append("\"children\":{" + childrenJson.mkString(",") + "},")
      sliceJsonBuilder.append("\"annotations\":{" + annotationsJson.mkString(",") + "}")
      sliceJsonBuilder.append("}")
      sliceJson = sliceJsonBuilder.toString
    // }

    (res.toString, crossings.toString, sliceJson, inSlice)
  }



  override def elaborate(c: Module): Unit = {
    super.elaborate(c)

        // System.err.println(m.line)
    // {
    //   val a = c.children(0)
      // System.err.println("----------------------------------------")
      // System.err.println(a.nodes)
      // System.err.println("----------------------------------------")
      // a.bfs(System.err.println(_))
      // System.err.println("----------------------------------------")
      // Driver.bfs(System.err.println(_))
      // System.err.println("----------------------------------------")
      // Driver.idfs(System.err.println(_))
      // System.err.println("----------------------------------------")
      // a.nodes.map(System.err.println(_))
      // System.err.println("----------------------------------------")
      // a.nodes.map((x: Node) => System.err.println(x.line))
      // System.err.println("----------------------------------------")
    // }

    flattenAll

    if (Driver.partitionIslands) {
      islands = createIslands()
    }
    var gn = -1;

    criteria = Driver.sliceCriteria.map((specification) => new Criterion(c, specification))

    val sliceNodeSets = Set[Set[Node]]()
    // val sliceLinksRaw = Map[Node,Map[Node,Set[StackTraceElement]]]()

    for (criterion <- criteria) {
      System.err.println("Founds node from criterion " + criterion.specification + " to be " + criterion.nodes.map((node) => node.name))
      seedNodes ++= criterion.nodes

      val nodeSlice = NodeSlice.fromCriterion(criterion)
      sliceNodeSets += nodeSlice.nodes
      // sliceLinksRaw ++= nodeSlice.links
    }
    if (sliceNodeSets.size > 0) {
      sliceNodes = sliceNodeSets.reduce((a, b) => a&b)
    } else {
      System.err.println("Warning: No slice sets")
      sliceNodes = Set()
    }
    System.err.println("Nodes in slice: " + sliceNodes.size)
    // for ((from, toset) <- sliceLinksRaw) {
    //   if (sliceNodes(from)) {
    //     for ((to, set) <- toset) {
    //       if (sliceNodes(to)) {
    //         sliceLinks(from) = Map(to, set)
    //       }
    //     }
    //   }
    // }

    val basedir = c.name + ".staticslice";
    ensureDir(basedir);
    ensureDir(basedir + "/styles");

    val out_d = createOutputFile(basedir + "/" + c.name + ".slice.dot")
    val out_slice = createOutputFile(basedir + "/slice.js")
    out_d.write("digraph " + c.name + "{\n")
    out_d.write("rankdir = LR;\n")
    val (innertext, innercrossings, sliceText, _) = emitModuleText(c, 0, basedir)
    out_d.write(innertext)
    if (Driver.partitionIslands && innercrossings.length > 0) {
      out_d.write(innercrossings)
    } else {
      Predef.assert(innercrossings.length == 0,
        ChiselError.error("Internal Error: length:" + innercrossings.length + ", " + innercrossings))
    }
    out_d.write("}")
    out_d.close()
    out_slice.write("var slice = ")
    out_slice.write(sliceText)
    out_slice.close()

    // Create the HTML pages for the source files
    for (sourceName <- requiredSourceFiles) {
      val escapedSourceName = xml.Utility.escape(sourceName);
      val source = scala.io.Source.fromFile(sourceName).mkString
      val escapedSource = xml.Utility.escape(source)

      val outHtml = createOutputFile(basedir + "/source_" + sourceName + ".html")
      outHtml.write("<!DOCTYPE html>\n<html>")

      outHtml.write("<head>")
      outHtml.write("<meta charset=\"UTF-8\">")
      outHtml.write("<title>" + escapedSourceName + " - Chisel Slice</title>")
      outHtml.write("<link rel=\"stylesheet\" href=\"styles/default.css\">")
      outHtml.write("<link rel=\"stylesheet\" href=\"styles/slice.css\">")
      outHtml.write("<script>var mode = 'source';</script>")
      outHtml.write("</head>")
      outHtml.write("<body>")
      outHtml.write("<h1 id=\"title\">" + escapedSourceName + "</h1>")
      outHtml.write("<h2 id=\"inst\"></h2>")
      outHtml.write("<noscript>You must enable javascript to view the slice!</noscript>")
      outHtml.write("<div id=\"content\"><p><pre id=\"source-pre\"><code id=\"source\">" + escapedSource + "</code></pre></p></div>")
      outHtml.write("<script src=\"highlight.pack.js\"></script>")
      outHtml.write("<script>hljs.initHighlightingOnLoad();</script>")
      outHtml.write("<script src=\"slice.js\"></script>")
      outHtml.write("<script src=\"parsing.js\"></script>")
      outHtml.write("<script src=\"slice_view.js\"></script>")
      outHtml.write("</body>")

      outHtml.write("</html>\n")
      outHtml.close()
    }

    // Create the HTML page for annotations
    {
      val outHtml = createOutputFile(basedir + "/annotation.html")
      outHtml.write("<!DOCTYPE html>\n<html>")

      outHtml.write("<head>")
      outHtml.write("<meta charset=\"UTF-8\">")
      outHtml.write("<title>Annotation - Chisel Slice</title>")
      outHtml.write("<link rel=\"stylesheet\" href=\"styles/slice.css\">")
      outHtml.write("<script>var mode = 'annotation';</script>")
      outHtml.write("</head>")
      outHtml.write("<body>")
      outHtml.write("<h1 id=\"title\">(Annotation)</h1>")
      outHtml.write("<h2 id=\"inst\"></h2>")
      outHtml.write("<noscript>You must enable javascript to view the slice!</noscript>")
      outHtml.write("<div id=\"content\"><p><pre id=\"annotation-pre\"><code id=\"source\">...</code></pre></p></div>")
      outHtml.write("<script src=\"slice.js\"></script>")
      outHtml.write("<script src=\"parsing.js\"></script>")
      outHtml.write("<script src=\"annotation_view.js\"></script>")
      outHtml.write("</body>")

      outHtml.write("</html>\n")
      outHtml.close()
    }

    val escapedSourceName = xml.Utility.escape(c.constructorLine.getFileName());
    val outHtml = createOutputFile(basedir + "/index.html")
    outHtml.write("<!DOCTYPE html>\n<html>")
    outHtml.write("<head>")
    outHtml.write("<meta charset=\"UTF-8\">")
    outHtml.write("<title>" + escapedSourceName + " - Chisel Slice</title>")
    outHtml.write("</head>")
    outHtml.write("<body>")
    outHtml.write("<p><a href=\"source_" + escapedSourceName + ".html\">" + "View slice</a></p>")
    outHtml.write("</body>")
    outHtml.write("</html>\n")
    outHtml.close()

    copyToTarget("parsing.js", basedir + "/parsing.js")
    copyToTarget("annotation_view.js", basedir + "/annotation_view.js")
    copyToTarget("slice_view.js", basedir + "/slice_view.js")
    copyToTarget("highlight.pack.js", basedir + "/highlight.pack.js")
    copyToTarget("default.css", basedir + "/styles/default.css")
    copyToTarget("slice.css", basedir + "/styles/slice.css")
  }
}

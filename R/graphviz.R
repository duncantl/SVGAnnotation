
#setMethod("svgPlot",  c(cmd = "Ragraph"),
svgPlot.graphviz =
function(filename, cmd, ..., asXML = FALSE, addIds = TRUE)
{
  grDevices::svg(filename)
  plot(cmd, ...)  # ... here or in svg()
  dev.off()

  doc = xmlParse(filename)
  if(addIds)
    addGraphIds(doc, cmd)
  if(asXML)
    doc
  else {
     saveXML(doc, filename)   
     filename
  }
}
#)


addGraphIds =
function(doc, graph,
          main = getNodeSet(doc, "//s:svg/s:g/s:g", c("s"= "http://www.w3.org/2000/svg"))[[1]])
{
  numNodes = length(graph@AgNode)
  nodeNames = sapply(graph@AgNode, slot, "name")
     # work on  pairs of SVG elements which are the circle/box for the node
     # and the node label.
  i = seq(1, length = numNodes, by = 2)
  mapply(function(x, val) addAttributes(x, id = val, class = "node"),
          xmlChildren(main)[i], nodeNames)
  mapply(function(x, val) addAttributes(x, id = val, class = "nodeLabel"),
          xmlChildren(main)[i + 1], paste(nodeNames, "label", sep = "-"))

    # Put the text of the node label on the element in the <defs> for that node.
    # This makes it easier for subsequent post-processing/annotation.
  ids = gsub("^#", "", sapply(xmlChildren(main)[i + 1], function(x) xmlGetAttr(x[["use"]], "href")))
  defs = xmlRoot(doc)[["defs"]]
  sapply(seq(along = ids), function(i) {
              el = getNodeSet(defs, paste(".//*[@id=", sQuote(ids[i]), "]"))
              addAttributes(el[[1]], text = nodeNames[i])
           })
  

  numEdges = length(graph@AgEdge)

  
    # Need to figure out how many SVG elements there are for each edge.
    #XX For now, just assume 1. Okay for twopi layout!
  numParts = sapply(graph@AgEdge, function(x) length(x@splines))
  if(!all(numParts == 1)) 
      groupEdgeSegments(main, graph, numParts, 2*numNodes)
  else {
    edgeNodes = xmlChildren(main)[  - seq(length = 2 * length(graph@AgNode)) ]
    if(numEdges != length(edgeNodes)) {
         gids = groupEdgeNodes(edgeNodes)
         tapply(edgeNodes, gids, igroupEdgeSegments, parent = main)
    }
  }

  numEls = xmlSize(main) - 2 * numNodes
  if(numEls != numEdges)
    warning("things going awry!")

  edgeIds = genEdgeIds(graph)
  edgeLabels = sapply(graph@AgEdge, function(x) if(length(x@txtLabel@labelText)) x@txtLabel@labelText  else "")
  edgeNodes = xmlChildren(main)[- seq(1, length = 2*numNodes) ]
  mapply(function(node, id, label) {
            addAttributes(node, id = id, label = label, class = "edge")
         },
        edgeNodes, edgeIds, edgeLabels)

  list(edgeIds = edgeIds, edgeLabels = edgeLabels, nodeIds = nodeNames)
#  invisible(doc)
}


groupEdgeNodes =
  #
  # Different from groupEdgeSegments below and used for finding
  # when the end of one edge is the same as the start of another edge.
  # Really want to check that this is not right on a node
  #
function(edgeNodes, shapes = lapply(edgeNodes, getShape))
{
   gid = 1L
   group = integer(length(shapes))
   group[1] = 1L
   for(i in 2:length(shapes)) {
      if(all(shapes[[i]][[1]] == shapes[[i-1]][[length(shapes[[i-1]])]]))
         group[i] = gid
      else {
        gid = gid + 1L
        group[i] = gid
      }
   }

   group
}


groupEdgeSegments =
function(gnode, graph, numParts = sapply(graph@AgEdge, function(x) length(x@splines)),
          start = 2 * length(graph@AgNode))
{
  if(start >= xmlSize(gnode))
    return()
  
  id = rep(1:length(numParts), numParts)
  edgeNodes = xmlChildren(gnode)[(start+1):xmlSize(gnode)]
  tapply(edgeNodes, id, igroupEdgeSegments, parent = gnode)
}

igroupEdgeSegments =
function(nodes, parent = xmlParent(nodes[[1]]))
{
  if(length(nodes) == 1)
    return()

  i = XML:::indexOfNode(nodes[[1]])
  removeNodes(nodes)
  g = newSVGNode("g", .children = nodes, parent = parent, at = i - 1L)

  g
}

setGeneric("genEdgeIds", 
function(graph)
            standardGeneric("genEdgeIds"))

setMethod("genEdgeIds", "Ragraph",
          function(graph)
{
  paste("edge", sapply(graph@AgEdge, function(x) paste(x@tail, x@head, sep = "-")), sep = ":")
})

setMethod("genEdgeIds", "graphNEL",
          function(graph)
{
    # Have to get the edge labels in the earlier-later order for undirected
  ans = getEdgeInfo(graph)
  paste("edge", unique(unlist(ans)), sep = ":")
})



getNodeElements =
function(doc)
{
  getNodeSet(doc, "/svg:svg/svg:g/svg:g//*[@class='node']", c(svg = SVG.namespaces[2]))
}

getEdgeElements =
function(doc)
{
  getNodeSet(doc, "/svg:svg/svg:g/svg:g//*[@class='edge']", c(svg = SVG.namespaces[2]))
}

getEdgeInfo =
function(graph, ...)
 standardGeneric("getEdgeInfo")

setMethod("getEdgeInfo", "graphNEL",
function(graph, ids = genEdgeIds(graph), ...) { #XXX wrong default value.

   # Need to make this more intelligent so it can handle undirected edges and
   # so find the ids w

  structure(lapply(seq(along = graph@edgeL),
                   function(i) {
                     edges = graph@edgeL[[i]]$edges
                     l = edges >= i
                     paste("edge:", c(if(any(l)) paste(graph@nodes[i], graph@nodes[edges[l]], sep = "-"),
                                      if(!all(l)) paste(graph@nodes[edges[!l]], graph@nodes[i], sep = "-")),
                            sep = "")
                    }), names = names(graph@edgeL))
})

#            nodeIds = sapply(graph@AgNode, slot, "name")
  


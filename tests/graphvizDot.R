library(SVGAnnotation)
if(require(graph) && require(Rgraphviz)) {

set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, 0.8)
x <- layoutGraph(g1)

xx = agopen(g1, layoutType = "dot", name = "bob")
doc = svgPlot(plot(xx))

ids = addGraphIds(doc, xx)


els = getNodeElements(doc)  # get the SVG elements corresponding to the nodes
sapply(seq(along = els), 
         function(i) 
            addAttributes(els[[i]], onmouseover = paste("highlightEdges(evt, ", i - 1, ", 'red');"),
                                    onmouseout = paste("highlightEdges(evt, ", i - 1, ");")))

  # Setup the "hiding" the other edges when we highlight these ones.
  #
info = getEdgeInfo(g1) # from the graph package.
otherEdges = lapply(info,
                     function(x) 
                       setdiff(ids$edgeIds, x))


  # add hyper-links to the nodes.
mapply(addLink, els, ids$nodeIds, silent = TRUE)


addECMAScripts(doc, system.file("examples", "JavaScript", "setEdgeStyle.js", package = "SVGAnnotation"), TRUE,
                  # These are R variables that are converted to JavaScript via our toJSON overloading.
               edgeTable = structure(info, names = NULL),
               edgeDiff = structure(otherEdges, names = NULL))


saveXML(doc, "graphviz1.svg")

}
##########################
### Code from exGraphviz.xml

library("XML")
library("SVGAnnotation")


library("Rgraphviz")
library("RJSONIO")
set.seed(123)
V = letters[1:10]
M = 1:4
g1 = randomGraph(V, M, 0.8)


doc = svgPlot(plot(g1, "twopi", 
              attrs = list(node = list(fillcolor = "white"))))


top = xmlRoot(doc)[["g"]][["g"]]
table(names(top))

NULL



layout2pi = agopen(g1, layoutType = "twopi", name = "bob")


#ids = addGraphIds(doc, layout2pi)


cat(toJSON(getEdgeInfo(g1)))

NULL



ids = addGraphIds(doc, layout2pi)
els = getNodeElements(doc)  
sapply(seq(along = els),
          function(i)
            addAttributes(els[[i]], 
             onmouseover = paste("highlightEdges(evt, ", i- 1, 
                                  ", 'chartreuse');"),
             onmouseout = paste("highlightEdges(evt, ", i-1, ");")
                          ))
info = getEdgeInfo(g1) 
names(info) = seq(from = 0, length = length(info))
otherEdges = lapply(info, function(x) setdiff(ids$edgeIds, x))
mapply(addLink, els, ids$nodeIds, MoreArgs = list(silent = TRUE))
jscript = c(system.file("examples", "Javascript", 
                        "highlightEdges.js", 
                         package = "SVGAnnotation"),
            system.file("examples", "Javascript", 
                        "setEdgeStyle.js",
                        package = "SVGAnnotation")
           )
addECMAScripts(doc, jscript, TRUE, edgeTable = info,
                 edgeDiff = otherEdges)
saveXML(doc, "graphviz.svg")




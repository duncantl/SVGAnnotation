# See graphviz.xml
if(require(graph) && require(Rgraphviz)) {

 library(XML)
 library(SVGAnnotation)
  
 set.seed(123)
 V <- letters[1:10]
 M <- 1:4
 g1 <- randomGraph(V, M, 0.8)
 x <- layoutGraph(g1)

 slotNames(x)

 gv = svgPlot(plot(x))
}

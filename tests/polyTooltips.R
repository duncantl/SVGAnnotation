library(SVGAnnotation)
if(require(maps)) {
 doc = svgPlot(map('state', fill = TRUE, col = "red"))
 p = getPlotPoints(doc)[[1]]
 addToolTips(p, as.character(1:63), addArea = FALSE)
 saveXML(doc, "/tmp/ttmap.svg")
}

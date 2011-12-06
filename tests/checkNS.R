library(SVGAnnotation)

doc = svgPlot(plot(1:10))
addToolTips(getPlotPoints(doc),  paste(1:10))

getNodeSet(doc, "//x:title", "x")

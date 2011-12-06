library(SVGAnnotation)

doc = svgPlot({
layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1), 3, ))
plot(1:100)
plot(10:1)
})

rr = getPlotRegionNodes(doc)
length(rr)
sapply(rr, xmlSize)

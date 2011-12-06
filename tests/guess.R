library(SVGAnnotation);library(lattice)
doc = svgPlot({plot(1:10); abline(v = 3); abline(h = 4)})
pts = getPlotPoints(doc)
guessSVGShape(pts[[1]][[1]])

sapply(pts[[2]], guessSVGShape)


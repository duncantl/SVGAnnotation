library(SVGAnnotation)
dd = list(a = c(rnorm(100), rnorm(10, 3)), b = rnorm(400))

doc1 = svgPlot(boxplot(dd$a))

pts = getPlotPoints(doc1)
shapes = sapply(pts, SVGAnnotation:::getShape)
sapply(shapes, class)


doc2 = svgPlot(boxplot(dd))

pts2 = getPlotPoints(doc2)
shapes = sapply(pts2, SVGAnnotation:::getShape)
sapply(shapes, class)

library(SVGAnnotation)

library(lattice)
 d = data.frame(x = runif(100), y = runif(100),
                type = sample(c("A", "B", "C"), 100, replace = TRUE),
                g = sample(c("w", "x", "y", "z"), 100, replace = TRUE))
 doc = svgPlot(xyplot(y ~ x | type, d, groups = g, auto.key = TRUE))
 nodes = getLatticeLegendNodes(doc)
 sapply(nodes, xmlGetAttr, "type")
 length(nodes) == 4 * 2

pp = svgPlot(xyplot( mpg ~ wt | am + cyl, mtcars, group = carb, auto.key = list(columns = 4, space ="right")))
nodes = getLatticeLegendNodes(pp)
sapply(nodes, xmlGetAttr, "type")
length(nodes) == 6 * 2

o = svgPlot(xyplot( mpg ~ wt | am + cyl, mtcars, group = carb, key = simpleKey(text = c("A", "B", "C"), points = FALSE, columns = 3)))
nodes = getLatticeLegendNodes(o)
sapply(nodes, xmlGetAttr, "type")
length(nodes) == 3


library(SVGAnnotation)

library(lattice)
 d = data.frame(x = runif(100), y = runif(100),
                type = sample(c("A", "B", "C"), 100, replace = TRUE),
                g = sample(c("w", "x", "y", "z"), 100, replace = TRUE))
 doc = svgPlot(xyplot(y ~ x | type, d, groups = g, auto.key = TRUE))
 nodes = getStripNodes(doc)
 length(nodes) == 3
 sapply(nodes, xmlAttrs)


 doc = svgPlot(xyplot(y ~ x | type + g, d) )
 nodes = getStripNodes(doc) 
 length(nodes) == 24
 sapply(nodes, xmlAttrs)


pp = svgPlot(xyplot( mpg ~ wt | am + cyl, mtcars, group = carb, auto.key = list(columns = 4, space ="right")))
 nodes = getStripNodes(pp)
 length(nodes) == 12
 sapply(nodes, xmlAttrs)

o = svgPlot(xyplot( mpg ~ wt | am, mtcars, group = carb, key = simpleKey(text = c("A", "B", "C"), points = FALSE, columns = 3)))
 nodes = getStripNodes(o)
 length(nodes) == 2
 sapply(nodes, xmlAttrs)  # Gets the variables wrong.


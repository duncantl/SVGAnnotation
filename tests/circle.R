library(SVGAnnotation)

doc = svgPlot(plot(5, 5, cex = 4, xlim = c(0,10), ylim = c(0,10)))
p = getPlotPoints(doc)[[1]]
box = getBoundingBox(p)
addSibling(p, newXMLNode("rect", attrs = c(x = box[1,1], y = box[1,2],
                                           width = box[2,1] - box[1,1], height = box[2,2] - box[1,2], fill = "blue")), after = FALSE)

saveXML(doc,  "circle.svg")

library(SVGAnnotation)
doc = svgPlot(plot(1:10, main = "The SVGAnnotation Package"))
nodes = getPlotPoints(doc)
addLink(nodes,
         paste("http://ww.r-project.org", seq(along = nodes), sep =""))


if(length(getPlotPoints(doc)) == 0) {
  stop("Failed")
} else
  saveXML(doc, "links.svg")

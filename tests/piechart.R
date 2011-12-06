library(SVGAnnotation)
load("browserUsage.rda")

# Structure of a pie chart is that the top-container <g id="surface0">
# has the arcs as pairs of children <g> and <g> with 
# the drawing of the arc itself and a line followed by a node 
# for the label for the arc

last = structure(as(webBrowsers[nrow(webBrowsers), - c( 1, 11, 12, 13) ], "numeric"), names = names(webBrowsers)[- c(1, 11, 12, 13)])
p = svgPlot(pie(last))

  # Returns a list of the arcs. Not quite what we want.
getPlotRegionNodes(p)
  #
getPlotPoints(p)


g = xmlRoot(p)[["g"]]
# skip the first node <rect>
arcs = xmlChildren(g)[-1]

i = seq(1, length(arcs) - 1, by = 2)
addToolTips(sapply(arcs[i], `[[`, 1L),  text = last)

SVGAnnotation:::addLink(arcs[i + 1], sprintf("http://www.omegahat.org/%s", names(last)))
                   
saveXML(p, "/tmp/pie.svg")

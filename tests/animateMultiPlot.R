#
# This version plots all the points and then removes all of them except those from the first time period
# but it gets their coordinates.
#
#

library(SVGAnnotation)

D = data.frame( x = c(1, 2, 3, 4), y = rep(6, 4), time = 1:4)

#xlim = getRange(D$x), ylim = getRange(D$y)
pp = svgPlot({plot(y ~ x, D)
              abline (h = 6, col = "lightgray", lty = 3)
              abline (v = 4, col = "lightgray", lty = 3)              
              })

points = getPlotPoints(pp)[[1]]

targets = sapply(points,
                  function(x)  {
                     bb = getBoundingBox(x)
                     paste(bb[1:2] - bb[3]/2, collapse = ",")
                 })

begin = c("0s", "3.3333s", "6.6666s")

location = getBoundingBox(points[[1]])
circ = newXMLNode("circle", attrs = c(x = location[1] + location[3], y = location[2] + location[3], r = location[3], style = xmlGetAttr(points[[1]], "style")))
mapply(function(from, to, begin) {
          newXMLNode("animateMotion", attrs = c(from = from, to = to, fill = "freeze", begin = begin, dur = "3.3333s"),
                        parent = circ)
       }, targets[-length(targets)], targets[-1], begin)

removeNodes(points[-1])
replaceNodes(points[[1]], circ)

loaded = !file.exists("/tmp/animateMultiPlot.svg")
saveXML(pp, "/tmp/animateMultiPlot.svg")
if(interactive() && !loaded)
  system("open -a opera /tmp/animateMultiPlot.svg")







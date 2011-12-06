library(SVGAnnotation)
source("gmInit.R")

 # works with no pch, '+' and 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 24,
 # others in between are just filled in versions of earlier ones, i.e. have a non-none stroke.
 # 7, 9, 12 works for green and blue, but not the red one because the conversion
 # from a Line to a RectangularBoundingBox mixed up the x and y and put x[1], y[1] in the x column.
 # So needed byrow = TRUE.
 # 
 # 3, 4 goes in funny directions.
 #     same problem as with 7, 9, 12, i.e. the Line -> RectangularBoundingBox.

 # 10, 13 fails with no method or default for coercing "Circle" to "RectangularBoundingBox"
 #  add coercion method.

 # So now 

 # 'A' can't specify a letter in groupByPch.
pch = c(7, 8, 10)
pp = svgPlot({plot(y ~ x, subset(D, time == 1), pch = pch, 
                   col = c("red", "green", "blue"),
                   bg = c("red", "green", "blue"),
                   xlim = range(D$x), ylim = range(D$y))
               with(D[D$time == 1,], points(x, y, col = c("red", "green", "blue")))
               with(D[D$time != 1,], points(x, y, col = 'lightgray'))              
               abline (h = c(2, 4, 6), col = "lightgray", lty = 3)
               abline (v = c(2, 7, 12), col = "lightgray", lty = 2)              
              })


rr = getPlotRegionNodes(pp)[[1]]
groupByPch(xmlChildren(rr), pch)

animate(pp, D, "time", dropFirst = TRUE, labels = seq(2005, length = 5),
          points = xmlChildren(rr), radii = "radius") 


saveXML(pp, "/tmp/gm4.svg")

if(interactive())
 system("open -a opera /tmp/gm4.svg")


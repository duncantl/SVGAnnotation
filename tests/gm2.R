source("gmInit.R")
#########################################

library(SVGAnnotation)

#xlim = range(D$x) + c(-1, 1)
#ylim = range(D$y) + c(-1, 1)

pp = svgPlot({plot(y ~ x, subset(D, time == 1), pch = "+",
                   col = c("red", "green", "blue"),
                   bg = c("red", "green", "blue"),
                   xlim = range(D$x), ylim = range(D$y))
               with(D[D$time == 1,], points(x, y, col = c("red", "green", "blue")))
               with(D[D$time != 1,], points(x, y, col = 'lightgray'))              
               abline (h = c(2, 4, 6), col = "lightgray", lty = 3)
               abline (v = c(2, 7, 12), col = "lightgray", lty = 2)              
              })

  # We get the plot's points elements ourselves and hand them to animate()
  # since currently getPlotPoints() and getPlotRegionNodes() don't recognize
  # these "funny" points.
animate(pp, D, "time", dropFirst = TRUE, labels = seq(2005, length = 5),
          points = xmlChildren(xmlRoot(pp)[["g"]][[2]])) # , radii = "radius")


saveXML(pp, "/tmp/gm2.svg")



###############
#  Use a different pch


pp = svgPlot({plot(y ~ x, subset(D, time == 1), pch = 5,
                   col = c("red", "green", "blue"),
                   bg = c("red", "green", "blue"),
                   xlim = range(D$x), ylim = range(D$y))
               with(D[D$time == 1,], points(x, y, col = c("red", "green", "blue")))
               with(D[D$time != 1,], points(x, y, col = 'lightgray'))              
               abline (h = c(2, 4, 6), col = "lightgray", lty = 3)
               abline (v = c(2, 7, 12), col = "lightgray", lty = 2)              
              })


animate(pp, D, "time", dropFirst = TRUE, labels = seq(2005, length = 5),
          points = xmlChildren(xmlRoot(pp)[["g"]][[2]])) # , radii = "radius")


saveXML(pp, "/tmp/gm3.svg")


###########



library(SVGAnnotation)

D = data.frame( x = c(1, 2, 3, 4), y = rep(6, 4), time = 1:4)

pp = svgPlot({plot(y ~ x, subset(D, time == 1),
                    pch = 2,  # triangle
                    xlim = range(D$x), ylim = range(D$y) 
                  )
               abline (h = c(6), col = "lightgray", lty = 3)
              })

animate(pp, D, "time", dropFirst = TRUE, labels = seq(2005, length = 4), radii = list( c(1.2, 1.5, 1.2)), start = 2)
saveXML(pp, "/tmp/simpleAnim1.svg")


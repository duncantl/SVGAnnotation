library(SVGAnnotation)
x = 1:100
y = 4 + .3*x + rnorm(length(x))

pp = svgPlot({
          plot(x[1], y[1], xlim = range(x), ylim = range(y), col = "red")
          lines(x, y, type = "l", col = "lightgrey", lty = 3)
        })

animate(pp, data.frame(x, y), "x", dropFirst = TRUE,
         labels = unique(x),
           begin = 1, radii = NULL)

saveXML(pp, "ts.svg")

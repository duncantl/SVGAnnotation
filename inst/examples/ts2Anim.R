library(SVGAnnotation)
x = 1:100
y1 = 4 + .1*x + rnorm(length(x), sd = 1.5)
y2 = 4 + .15*x + rnorm(length(x), sd = 2)

pp = svgPlot({
          plot(c(x[1], x[1]), c(y1[1], y2[1]), xlim = range(x), ylim = range(c(y1, y2)),
                 col = c("red", "blue"), xlab = "time", ylab = "value",
               main = "2 correlated time series")
          lines(x, y1, type = "l", col = "#FF000099", lty = 3)
          lines(x, y2, type = "l", col = "#0000FF99", lty = 3)
        })

D = data.frame(x = c(x, x), y = c(y1, y2), id = rep(c(1, 2), each = length(x)))

animate(pp, D, "x", dropFirst = TRUE,
         labels = unique(x), 
           begin = 1, radii = NULL)

saveXML(pp, "ts2.svg")

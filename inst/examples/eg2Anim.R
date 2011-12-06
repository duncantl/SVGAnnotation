
p1 = matrix(
       c(10, 10,
         20, 10,
         30, 10,
         20,  5,
         10,  1,
         20, 1,
         30, 1), , 2, byrow = TRUE)


p2 = matrix(
       c(10, 10,
         20, 10,
         30, 10,
         20,  5,
         10,  1,
         20, 1,
         30, 1), , 2, byrow = TRUE)


D = data.frame(rbind(p1, p1[nrow(p1):1,]))
names(D) = c("x", "y")

D$radius = rep(2, nrow(D))
D$time = rep(1:nrow(p1), 2)
D$id = c(rep(1, nrow(p1)), rep(2, nrow(p1)))



library(SVGAnnotation)

   # We specify the xlim and ylim to ensure the other
   # points in time will
pp = svgPlot({plot(y ~ x, subset(D, time == 1),
                   pch = 21,
                   col = c("red", "blue"),
                   bg = c("red", "blue"),
                   xlim = range(D$x), ylim = range(D$y)
                  )
                    # Add horizontal and vertical lines at suitable positions.
               abline(h = unique(D$y), col = "lightgray", lty = 3)
               abline(v = unique(D$x), col = "lightgray", lty = 3)
              })

animate(pp, D, "time", dropFirst = TRUE,
         labels = unique(D$time) + 2000,
           begin = 1, radii = "radius")

saveXML(pp, "gm2.svg")

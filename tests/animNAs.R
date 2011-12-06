library(SVGAnnotation)

D = data.frame( x = c(1, 2, NA, NA, 5, 6), y = rep(4, 6), time = 1:6)

pp = svgPlot({plot(y ~ x, subset(D, time == 1),
                    xlim = range(D$x, na.rm = TRUE), ylim = range(D$y, na.rm = TRUE)   
#                xlim =c(0.568, 1.432),ylim = c(3.408, 8.592), xaxs = "i", yaxs="i"
               )

               points(D$x, D$y, col = 'lightgray')
               abline (h = c(4), col = "lightgray", lty = 3)
              })

animate(pp, D, "time", dropFirst = TRUE, labels = seq(2005, length = length(unique(D$time))))
saveXML(pp, "/tmp/animNAs.svg")

if(interactive() && Sys.info()["sysname"] == "Darwin")
  system("open -a opera /tmp/animNAs.svg")


#######################################


D = data.frame( x = c(1, NA, 2, NA, 5, 6, 1:6), y = c(rep(4, 6), 1:6), time = 1:6)

#D = do.call("rbind", by(oD, oD$time, function(x) x))

pp = svgPlot({plot(y ~ x, subset(D, time == 1),
                    xlim = range(D$x, na.rm = TRUE), ylim = range(D$y, na.rm = TRUE)   
               )

               points(D$x, D$y, col = 'lightgray')
               abline (h = c(4), col = "lightgray", lty = 3)
              })

animate(pp, D, "time", dropFirst = TRUE, labels = seq(2005, length = length(unique(D$time))))
saveXML(pp, "/tmp/animNAs1.svg")


if(interactive() && Sys.info()["sysname"] == "Darwin")
  system("open -a opera /tmp/animNAs1.svg")



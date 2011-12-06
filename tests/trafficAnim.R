# This is almost identical to exScatterplotAnimation.R byt
# it displays the color and weekday name in a legend so we can
# verify that they are the same.
library(SVGAnnotation)

traffic = read.table("traffic2009-01-04.txt",
                     header = TRUE, stringsAsFactor = FALSE)

traffic$time = as.Date(paste(traffic$Date, traffic$Timestamp), "%m/%d/%Y") 

colors = rainbow(7)
names(colors) = unique(weekdays(traffic$time))
xrange = range(traffic$Occ1)
yrange = range(traffic$Flow1)

# numer of observations per day (one each 5 minutes)
day1 = 1:(12*24)


doc = svgPlot({plot(Flow1 ~ Occ1, traffic[day1, ], xlim = xrange, ylim = yrange, 
                   main = "Loop Detector #312439 I 80E Sacramento", 
                   col=colors[1], pch = 19)
                legend(.25, 60, names(colors), col = colors, lty = 1, lwd = 3)
               })

# Now we can animate
z = animate(doc, traffic[, c("Occ1", "Flow1")], weekdays(traffic$time),
            inteval = 2, colors = colors[-1], labels = unique(weekdays(traffic$time)))


saveXML(doc, "trafficAnim.svg")

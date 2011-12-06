library(SVGAnnotation)

#traffic = read.table("traffic2009-01-04.txt",
#                     header = TRUE, stringsAsFactor = FALSE)

data(traffic)

traffic$time = as.Date(paste(traffic$Date, traffic$Timestamp), "%m/%d/%Y") 

colors = rainbow(7)

xrange = range(traffic$Occ1)
yrange = range(traffic$Flow1)


# numer of observations per day (one each 5 minutes)
day1 = 1:(12*24)


doc = svgPlot({ plot(Flow1 ~ Occ1, traffic[day1, ], xlim = xrange, ylim = yrange, 
                     main = "Loop Detector #312439 I 80E Sacramento", 
                     col=colors[1], pch = 19)
                legend(.25, 60, unique(weekdays(traffic$time)), col = colors, lty = 1, lwd = 3)                
              })

# Now we can animate
animate(doc, traffic[, c("Occ1", "Flow1")], weekdays(traffic$time), begin = 2,
         dur = "12s", colors = colors[-1], labels = unique(weekdays(traffic$time)))



 # Display the day of the week.

root = xmlRoot(doc)
days = unique(weekdays(traffic$time))

colors = substring(colors, 1, 7)
step = 2

sapply(seq(along = days),
   function(i) {
     node = newXMLNode("text", 
                        attrs = c(x = 10, y = 20,
                         style = paste("visibility: ",
                                 "hidden",
                                 "; stroke:", colors[i], ";")),
                        days[i], parent = root)
     newXMLNode("set", 
                attrs = c(attributeName = "visibility", attributeType="CSS",
                          fill = if(i  == length(days)) "freeze" else "no",
                          to = "visible",
                          begin = paste(step * (i - 1), "s", sep = ""),  
                          dur = step), 
                parent = node)
       })



addCSS(doc)

saveXML(doc, "occ_anim.svg")

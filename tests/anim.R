# See time.xml

library(SVGAnnotation)
  
load(url("http://www.omegahat.org/SVGAnnotation/tests/myTraffic.rda"))
traffic$time = as.POSIXct(as.Date("02/27/2004 00:01", "%m/%d/%Y %H:%S")) + 
                 seq(0, by = 5*60, length = nrow(traffic))
traffic = traffic[1:(6 * 24 * 12), ]
traffic$interval = rep(1:(24 * 60 /5), 6)

   # Create the plot for the first time period.
doc = svgPlot(plot(Flow1 ~ Occ1, traffic[1:288,], xlim = range(traffic$Occ1), ylim = range(traffic$Flow1), main = "Flow versus Occupancy"))

    # Now we can animate
z = animate(doc, traffic[, c("Occ1", "Flow1")], weekdays(traffic$time), dur = "10s")



ax = getAxesLabelNodes(doc)
tips = c("Freeway traffic for highway 80 in California",
          "Proportion of the 5 minute period the sensor was covered by a car",
           "Number of cars that passed over the sensor within the 5 minute period")
invisible(sapply(seq(along = ax), function(i) addToolTips(ax[[i]], tips[[i]])))


 # Display the day of the week.

root = xmlRoot(doc)
days = unique(weekdays(traffic$time))
 # Drop the alpha part of the color so get the # and the next 6 characters
colors = substring(c("#000000", rainbow( length(days) - 1 )), 1, 7)

step = 2.66666666666667

sapply(seq(along = days),
       function(i) {
           node = newXMLNode("text", attrs = c(x = 10, y = 20,
                                                style = paste("visibility:", if(i == 1) " visible" else "hidden",
                                                               "; stroke:", colors[i])),
                          days[i], parent = root)
           newXMLNode("set", attrs = c(attributeName = "visibility", attributeType="CSS",
                                       fill = if(i == 1 || i  == length(days)) "freeze" else "no",
                                       to = if(i != 1) "visible" else "hidden",
                                       begin = paste(step * (i - 1), "s", sep = ""),  dur = step), parent = node)
       })



addCSS(doc)

saveXML(doc, "occ_anim.svg")

# Want a legend which shows the days of the weeks in the colors we use.

# Also want the changing of the colors to start after an initial duration
# so we can see the original plot. It looks like it is supposed to.


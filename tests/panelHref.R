library(SVGAnnotation);library(lattice)
mtcars$am = factor( c("manual", "automatic")[mtcars$am + 1])
mtcars$gear = ordered( as.character(mtcars$gear), levels = c("3", "4", "5"))

o = svgPlot(xyplot( mpg ~ wt | am, mtcars, group = carb))
p = getPlotRegionNodes(o)

addToolTips(p, c("Panel 1", "Panel 2"))
addCSS(o)
saveXML(o, "/tmp/latticeToolTips.svg")


###################################################


o = svgPlot(xyplot( mpg ~ wt | am, mtcars, group = carb))
p = getPlotRegionNodes(o)

addLink(p[[1]], "http://www.omegahat.org", TRUE)

saveXML(o, "/tmp/latticePanelLink.svg")


#  box = getBoundingBox(p[[1]])

###############################################

o = svgPlot({
               par(mfrow = c(2, 1))
               plot(mpg ~ wt, mtcars)
               plot(mpg ~ disp, mtcars)
            })
p = getPlotRegionNodes(o)

addToolTips(p, c("Plot 1", "Plot 2"))

saveXML(o, "/tmp/mfrowToolTips.svg")

##################################################


xplot = xyplot( mpg ~ wt | am + gear, mtcars, group = carb)
o = svgPlot(xplot)
p = getPlotRegionNodes(o) # !!! Gets this wrong because of no observations in 2 panels.

tips = as.character(t(with(mtcars, outer(levels(gear), levels(am), function(x,y) paste("gear =", x, ", am =", y)))))
numEls = sapply(xplot$panel.args, function(x) length(x$x))
tips = paste(tips, paste(numEls, "observations"), sep = ": ")
addToolTips(p, tips, addArea = TRUE)

strip = getStripNodes(o)
addToolTips(strip[[1]], "Strip 1", addArea = FALSE)

addToolTips(strip, paste("Strip", 1:length(strip)), addArea = FALSE)

addLink(strip[[1]], "http://www.omegahat.org", FALSE)


saveXML(o, "/tmp/latticeStripToolTips.svg")

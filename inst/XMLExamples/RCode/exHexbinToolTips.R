##########################
### Code from exHexbinToolTips.xml

library("SVGAnnotation")
data("traffic")
Occupancy = unlist(traffic[ c("Occ1", "Occ2", "Occ3")])
Flow = unlist(traffic[c("Flow1", "Flow2", "Flow3")])


library("hexbin")
hbin = hexbin(Occupancy, Flow)
doc = svgPlot(
        plot(hbin, 
             main = "Loop Detector #313111 on I-80E Sacramento"))


ptz = getPlotPoints(doc)
length(ptz)

NULL

length(hbin@count)

NULL

tips = paste("Count: ", hbin@count)
addToolTips(ptz, tips, addArea = TRUE)


saveXML(doc, "hexbin.svg")




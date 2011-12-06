# Check that we can get back the axes label nodes and plot points
# if we annotate them previously.

library(SVGAnnotation)

# Create the basic plot
doc = svgPlot(plot(mpg ~ wt, mtcars, 
                main = "Motor Trend Car Road Tests", pch=19, col= "#377EB8"))

ax = getAxesLabelNodes(doc)

 # Add link from title to motor trend website
addLink(ax[[1]], "http://www.motortrend.com") 

 # Add tooltips to the x, y axes
tips = c("Weight in units of lb/1000", "Miles per US gallon")
addToolTips(ax[-1], tips)

length(getAxesLabelNodes(doc)) == length(ax)
length(getPlotPoints(doc)) == 32


######

doc = svgPlot(plot(1:10, pch = 8))
groupByPch(getPlotPoints(doc), 8)
length(getPlotPoints(doc))
rr = getPlotRegionNodes(doc)  # works
xmlSize(rr[[1]]) == 10



#####

doc = svgPlot(plot(1:10))
addToolTips(getPlotPoints(doc), 1:10)
getPlotPoints(doc)





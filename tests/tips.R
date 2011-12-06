library(SVGAnnotation)

         # Create the basic plot
svg("axes_tips.svg")
plot(mpg ~ wt, mtcars, main = "The title")
text(median(mtcars$wt), median(mtcars$mpg), "the medians")
dev.off()

 # Post process it
library(XML)
library(SVGAnnotation)

doc = xmlParse("axes_tips.svg")

addCSS(doc)

 # Add tooltips to the
ax = getAxesLabelNodes(doc)
tips = c("1974 Motor Trend US magazine fuel consumption data",
          "Weight in units of lb/1000", "Miles per US gallon")
invisible(sapply(seq(along = ax), function(i) addToolTips(ax[[i]], tips[[i]])))

 # tooltips for the points. The text is the name = value sequence for all the variables for that point.
addToolTips(doc, apply(mtcars, 1, function(x) paste(names(mtcars), x, sep = " = ", collapse = ", ")), doc = doc)

saveXML(doc, "axes_tips.svg")


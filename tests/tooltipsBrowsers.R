library(SVGAnnotation)

doc = svgPlot(plot(mpg ~ wt, mtcars))
addToolTips(getAxesLabelNodes(doc), c("Weight", "Miles per US Gallon"),
              addArea = 2, addTitleAttribute = TRUE)
addToolTips(doc, rownames(mtcars),  addArea = 2,
              style = "fill: white; stroke: red;", addTitleAttribute = TRUE)

addCSS(doc)
saveXML(doc, "tooltips.svg")


if(FALSE) {
system("open -a firefox tooltips.svg") # Okay
system("open -a opera tooltips.svg")  # Okay

  # points aren't working in WebKit.
system("open -a safari tooltips.svg")
system("open -a 'Google Chrome.app' tooltips.svg")  
}

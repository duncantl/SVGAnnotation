library(SVGAnnotation)
if(withLine) {
  doc = svgPlot({plot(mpg ~ wt, mtcars)
               abline(v=3)})
} else 
    doc = svgPlot(plot(mpg ~ wt, mtcars))

#addToolTips(getAxesLabelNodes(doc), c("Weight", "Miles per US Gallon"),
#            addArea = TRUE)

addToolTips(doc, 1:nrow(mtcars), #rownames(mtcars),
            addArea = TRUE,
            style = "fill: white; stroke: red;")

#addToolTips(getPlotPoints(doc)[[1]], 1:nrow(mtcars), #rownames(mtcars),
#            addArea = TRUE,
#            style = "fill: white; stroke: red;")

saveXML(doc, "doc.svg") 

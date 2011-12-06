library(SVGAnnotation)
cat("addECMA.R\n")
doc = svgPlot(plot(1:10))
addECMAScripts(doc, "inst/JavaScript/link.js", TRUE)
xmlRoot(doc)[[1]]

doc = htmlParse("<html><head></head><body></body>")
addECMAScripts(doc, I('function foo() { alert("Hi there");}"'))



if(FALSE) {
library(MASS) ; library(SVGAnnotation)
doc = svgPlot(parcoord(mtcars))
highlightMatplot(doc, color = 'rgb(100%,0%,0%)')
saveXML(doc, "parcoords_mtcars.svg")
}

getMatplotSeries = getParcoordSeries =
function(doc, paths = getNodeSet(doc, "//x:g[@id and starts-with(@id, 'surface')]//x:path", "x"))
{
  pline.p = sapply(paths, function(x) is(getShape(x), "Polyline"))
  paths[pline.p]
}


highlightMatplot =
function(doc,
         color = "null", factor = 3,
         series = getMatplotSeries(doc), ids = seq(along = series),
         js = system.file("JavaScript", "imatplot.js", package = "SVGAnnotation"))
{
  addECMAScripts(doc, js, color = color, factor = factor)
  invisible(mapply(highlightMatplotSeries, series, ids))
}

highlightMatplotSeries =
function(node, id)
{
  ops = sprintf('toggleSeries("%s", %s)', id, c("true", "false"))
  names(ops) = c("onmouseover", "onmouseout")
  xmlAttrs(node)  <- c(id = id, ops)
  convertCSSStylesToSVG(node)  
}

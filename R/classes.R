setOldClass(c("XMLPlot", extends("XMLInternalDocument")))
setOldClass(c("GRZXMLPlot", extends("XMLPlot")))
setOldClass(c("ScatterXMLPlot", extends("GRZXMLPlot")))
setOldClass(c("HistogramXMLPlot", extends("GRZXMLPlot")))
setOldClass(c("BarchartXMLPlot", extends("GRZXMLPlot")))
setOldClass(c("TimeSeriesXMLPlot", extends("GRZXMLPlot")))
setOldClass(c("PairsXMLPlot", extends("GRZXMLPlot")))

setOldClass(c("LatticeXMLPlot", extends("XMLPlot")))
setOldClass(c("XYXMLPlot", extends("LatticeXMLPlot")))
setOldClass(c("DensityXMLPlot", extends("LatticeXMLPlot")))


xmlPlot =
  #
  # extend to "recognize" scatterplot, barchart,
  #  lattice plots, etc.
  #
function(file, class = character())
{
   doc = xmlParse(file)
   class(doc) = c(class, class(doc))
   doc
}


histogramPlot =
function(file, doc  = xmlParse(file), class = "HistogramXMLPlot")
{
  class(doc) = extends(class)
  doc
}

scatterPlot =
function(file, doc  = xmlParse(file), class = "ScatterXMLPlot")
{
  class(doc) = extends(class)
  doc
}

pairsPlot =
function(file, doc  = xmlParse(file), class = "PairsXMLPlot")
{
  class(doc) = extends(class)
  doc
}

if(FALSE) 
setGeneric("svgPlot",
function(filename, cmd, ...)
{
  expr = substitute(cmd)
  standardGeneric("svgPlot")
})


\name{getPlotRegionNodes}
\alias{getPlotRegionNodes}
\alias{getStripNodes}
\alias{getPanelDataNodes}
\alias{getPlotPoints}
\alias{getTextNodes}
\alias{getPlotPoints,XMLInternalNode-method}
\alias{getPlotPoints,XMLInternalDocument-method}
\alias{getTextPoints}
\title{Get the nodes within an SVG document which contain different sub-components}
\description{
  \code{getPlotRegionNodes} gets the XML/SVG nodes 
  within the document which house the contents of the data regions
  of the sub-plots within the R graphic.
  This works for traditional and lattice plots,
  and also for histograms and boxplots.

 \code{getStripNodes} gets the nodes in a lattice plot
  that represent the strips above each panel.
 Currently doesn't do anything!

% \code{getPanelDataNodes}
  \code{getPlotPoints} is intended to get the
  elements in the SVG document that correspond
  to regular data points in the data region of the
  plots/sub-plots.

  \code{getTextNodes} returns the nodes in the XML
  document that correspond to text in the figure.
}
\usage{
getPlotRegionNodes(doc, lattice = FALSE,
                    hasAnnotations = lattice, addTypes = TRUE,
                     isRegion = isPlotRegionNew, ...)
getTextPoints(doc, ...)
getTextNodes(doc, ...)
getPlotPoints(doc, simplify = TRUE, addTypes = TRUE, ...)
}
\arguments{
  \item{doc}{the parsed SVG document}
  \item{lattice}{ a logical value indicating whether the plot is a
    trellis/lattice plot.}
  \item{hasAnnotations}{whether to use a heuristic which will
     recognize/permit the presence of additional R-level elements
      (e.g. text, lines) annotations within the
    plot region children.}
  \item{\dots}{any additional parameters}
  \item{simplify}{a logical value. If this is \code{TRUE}, then if the
  SVG document only contains a single plot region, the points in that
  plot region are returned as a list.  However, if this is \code{FALSE},
  then the return value is a list with as many elements as there are
  plot regions in the SVG document. Each element is a list of the SVG
  nodes corresponding to those points. So, in short,
  \code{TRUE} is useful for interactive use as it strips away the outer
  list that contains a single point. But for programmatic use,
  when we don't know how many plot regions there are, it is best
  to get back a the structured list.}
\item{addTypes}{a logical value which if \code{TRUE} causes the function
  to add a \code{type} attribute to the SVG elements it returns that
  identifies the high-level nature or type of the node,
  e.g. 'plot-point', 'plot-region', 'strip', ...}

  \item{isRegion}{the function that is used to determing if a \code{<g>}
     element is an actual plotting region.}
}
\value{
 \code{getPlotRegionNodes} returns a list of the group (g) nodes which have only path children
 and discards those that are the entire device region.
  
 \code{getStripNodes} returns a list of nodes.
}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{svgPlot}}
}
\examples{

 doc = svgPlot({
                par(mfrow = c(3, 1))
                plot(mpg ~ wt, mtcars)
                hist(mtcars$mpg)
                plot(mpg ~ disp, mtcars)
               })

  plots = getPlotRegionNodes(doc)
  length(plots)


  doc = svgPlot(pairs(mtcars))
  rr = getPlotRegionNodes(doc)
  length(rr)
  sapply(rr, xmlSize)

     # Using layout() with 2 plots and the second as an inset in the first.
  doc = svgPlot({
          layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1), 3, ))
          plot(1:100)
          plot(10:1)
         })

   rr = getPlotRegionNodes(doc)
   length(rr)
   sapply(rr, xmlSize)


  library(lattice)
  pp = svgPlot(xyplot( mpg ~ wt | am + cyl, mtcars, group = carb,
                        auto.key = list(columns = 4, space ="right")))
    # Note that the order of the plot regions is different from that in
    # the lattice/trellis object $panel.args
  a = getPlotRegionNodes(pp)
  lapply(a, getPlotPoints)
  getPlotPoints(pp)

  nodes = getLatticeLegendNodes(pp)
  sapply(nodes, xmlGetAttr, "type")


  # Draw a series of line segments. Note that this only appears as a
  # single observation.
  dd = svgPlot(plot(1:10, type = "l"))
  rr = getPlotRegionNodes(dd)
  xmlSApply(rr[[1]], guessSVGShape) 

   # Here we draw both lines and the points.
   # We end up with 10 points and 10 lines all inside the same <g>
  dd = svgPlot(plot(1:10, type = "b"))
  rr = getPlotRegionNodes(dd)
  xmlSize(rr[[1]])
  xmlSApply(rr[[1]], guessSVGShape)

   # Here we draw both lines and the points but separately.
   # We end up with 2 "plotting regions", i.e. <g> nodes and the lines
   # are in the second one.
   # This plot is slightly different with the lines running through the points.
  dd = svgPlot({ plot(1:10)
                 lines(1:10, 1:10)
               })
  rr = getPlotRegionNodes(dd)
  length(rr)
  xmlSize(rr[[1]])
  xmlSize(rr[[2]])

  isInside(rr[[2]], rr[[1]])

  xmlSApply(rr[[2]], guessSVGShape)
     #  a Polyline.
}

\keyword{iplot}
\keyword{aplot}
\keyword{dynamic}
\keyword{device}


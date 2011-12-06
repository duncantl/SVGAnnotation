\name{guessSVGShape}
\Rdversion{1.1}
\alias{guessSVGShape}

\title{Determine the shape for an SVG element}
\description{
  This function and its methods  attempt to determine the
  shape corresponding to  an SVG element.
}
\usage{
guessSVGShape(doc, addShapes = TRUE, recursive = FALSE, ...)
}
\arguments{
  \item{doc}{the SVG element or the document or list of nodes}
  \item{addShapes}{a logical value that determines whether we add a
    \code{shape} attribute to the node with the determined value.  This
    is useful for post-processing the SVG document}
  \item{recursive}{a logical value. Currently ignored. In the future it
    will control whether child nodes are also processed.}
  \item{\dots}{additional parameters for the methods}
}
\value{
  A character vector giving the names of the determined shapes.
  If \code{addShapes} is \code{TRUE}, the XML node(s) is modified too
  as a side effect.
}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{getPlotRegionNodes}}
  \code{\link{getPlotPoints}}
  \code{\link{getAxesLabelNodes}}  
}
\examples{

  dd = svgPlot(plot(1:10, type = "l"))
  rr = getPlotRegionNodes(dd)
  xmlSApply(rr[[1]], guessSVGShape)

  dd = svgPlot(plot(1:10, type = "b"))
  rr = getPlotRegionNodes(dd)
  xmlSize(rr[[1]])
  xmlSApply(rr[[1]], guessSVGShape)


  dd = svgPlot(plot(0:25, pch = 0:25))
  rr = getPlotRegionNodes(dd)
  xmlSApply(rr[[1]], guessSVGShape)
}
\keyword{programming}
\keyword{aplot}
\keyword{dplot}

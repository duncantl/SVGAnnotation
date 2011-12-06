\name{getShape}
\Rdversion{1.1}
\alias{getShape}
\title{Compute the shape of an SVG node}
\description{
  This function analyzes a given SVG node and attempts to determine
  the shape of the element.  It then returns an object of the
  corresponding shape class (e.g. Circle, Line, VerticalLine, ...)
  containing the relevant information about that shape for this element,
  e.g. the x, y coordinates and the radius for a circle.
}
\usage{
getShape(node, d = xmlGetAttr(node, "d"))
}
\arguments{
  \item{node}{the SVG node}
  \item{d}{the path of the node that draws the shape.}
}
\value{
  An object of one of the classes derived from the class \code{Shape}.
  These are Circle, Polygon, Polyline, Line, RectangularBoundingBox,
  VerticalLine and HorizontalLine.
}
\references{
}
\author{
Duncan Temple Lang
}

\seealso{
 \code{\link{guessSVGShape}}. This is probably the more appropriate
 function to use.
}
\examples{

dd = list(a = c(rnorm(100), rnorm(10, 3)), b = rnorm(400))

doc1 = svgPlot(boxplot(dd$a))

pts = getPlotPoints(doc1)
shapes = sapply(pts, SVGAnnotation:::getShape)
sapply(shapes, class)


doc2 = svgPlot(boxplot(dd))

pts2 = getPlotPoints(doc2)
shapes = sapply(pts2, SVGAnnotation:::getShape)
sapply(shapes, class)

}
\keyword{iplot}
\keyword{aplot}
\keyword{dynamic}
\keyword{device}
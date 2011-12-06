\name{getViewBox}
\alias{getViewBox}
\title{Get the dimensions of the viewing area/box of an SVG document}
\description{
  This queries the SVG document to fetch and parse the \code{viewBox}
  attribute of the root \code{svg} node.
}
\usage{
getViewBox(doc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the parsed XML/SVG document}
}

\value{
  It returns a 2-by-2 matrix giving the x and y coordinates in the
  first and second columns, with the top being in the first row.  
}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{enlargeSVGViewBox}}
  \code{\link{dim,SVGDocument-method}}  
}
\examples{
 doc = svgPlot(plot(1:10))
 getViewBox(doc)
 dim(doc)

 doc = svgPlot(plot(1:10), width = 8, height = 11.5)
 getViewBox(doc)
 dim(doc)
}
\keyword{hplot}

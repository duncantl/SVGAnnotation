\name{enlargeSVGViewBox}
\alias{enlargeSVGViewBox}
\title{Enlarge the viewing area/dimensions of an SVG file}
\description{
  This function is a high-level interface to changing the dimensions
 of  the SVG document.  It is mostly used, so far, to enlarge the
 view of an SVG display created in R so that we can add extra
  elements when post-processing the document. For example, to add 
  a slider below on the side of a plot we must enlarge the 
  viewing area in the appropriate dimension.
}
\usage{
enlargeSVGViewBox(doc, x = 0, y = 0, svg = xmlRoot(doc))
}
\arguments{
  \item{doc}{the parsed XML/SVG document}
  \item{x}{the amount to add to the width of the SVG document's viewing area}
  \item{y}{the amount to add to the height of the SVG document's viewing area}
  \item{svg}{the root node whose \code{width} and \code{height} attributes are to be modified}
}
\value{
 The updated SVG document, invisibly.
}
\references{
 \url{http://www.carto.net/papers/svg/samples/index.shtml#iact}
}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{addSlider}}
}
\examples{
  doc = svgPlot(plot(1:10))
  enlargeSVGViewBox(doc, 100, 200)
  g = xmlRoot(doc)[["g"]]
  newXMLNode("text", "Horizontal",
              attrs = c(x = 250, y  = 600, fill = "red"),
             parent = g)
  newXMLNode("text", "Vertical",
              attrs = c(x = 550, y  = 250, fill = "blue"),
             parent = g)
}
\keyword{hplot}
\keyword{iplot}



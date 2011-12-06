\name{isInside}
\Rdversion{1.1}
\alias{isInside}
\alias{isInside,Line,RectangularBoundingBox-method}
\alias{isInside,Circle,RectangularBoundingBox-method}
\alias{isInside,Polyline,RectangularBoundingBox-method}
\alias{isInside,RectangularBoundingBox,RectangularBoundingBox-method}
\alias{isInside,XMLInternalNode,RectangularBoundingBox-method}
\alias{isInside,XMLInternalNode,XMLInternalNode-method}

\title{Determine if one SVG object is physically located within another}
\description{
  This function and its methods allow us to query whether the bounding
  box or area of one SVG element is inside that of another.
}
\usage{
isInside(obj, host)
}
\arguments{
  \item{obj}{the object whose area might be inside the bounding box of \code{host}}
  \item{host}{the element whose area might enclose that of \code{obj}}
}
\value{
  A logical value with \code{TRUE} indicating \code{obj}
  is "inside" \code{host}, in a physical sense on the SVG display.
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{getBoundingBox}}
  \code{\link{isAnnotationGroup}}
  \code{\link{getGeneralPath}}  
}
\examples{
doc = svgPlot({
           plot(1:10, pch = 21, fg = c("red"), bg = "lightgrey")

           points(1:10, pch = '+')

           abline(v = c(3, 7), col = 'blue', lty = 3)
           abline(h = c(3, 7, 9), col = 'lightgray', lty = 2)           
        })
g = xmlRoot(doc)[["g"]][[2]]
rr = getPlotRegionNodes(doc)

xmlSApply(rr[[2]], isInside, rr[[1]])

  # Compute the bounding box for the potential host/container
  # just once and pass it to each call to avoid it being computed each time.
 bb = getBoundingBox(rr[[1]])
 xmlSApply(rr[[2]], isInside, bb)

\dontrun{
    # no method for this yet.
  # Check if the first + is inside the first circle.
 isInside(rr[[2]][[1]], rr[[1]][[1]])
}

}
\keyword{programming}
\keyword{dplot}

\name{getBoundingBox}
\alias{getBoundingBox}
\alias{getRect}
\alias{getGeneralPath}
\alias{getPanelCoordinates}
\alias{getPlotRegion}
\title{Functions for finding rectangles associated with SVG objects}
\description{
  These functions process the path specifications in an SVG node,
  i.e. the \code{d} attribute.

  The most general is \code{getGeneralPath} which breaks the path
  string into a list with as many elements as there are path operations
  (i.e. M, L, Z, C, S, H, V, ...)
  and each element containing the numbers for that operation.
  From this, we can determine the shape, the bounding box, etc.

  The other functions compute the rectangle enclosing or associated with
  an SVG object such as an axis label or any text,
  a plotting region and so on.

  This is useful when creating a link or a mouse over event for an SVG element
  which is just a path.   Ordinarily, the action will only be active
  when the mouse is on the path rather than "in" the object.
  By putting a rectangle behind the object and placing the action on
  that   rectangle, the action is active when the mouse is "within"
  the object, not just on the path.
  
  See \code{\link{addLink}} and \code{\link{addToolTips}} and their
  \code{addArea} parameter.
}
\usage{
getBoundingBox(gnode, doc = as(gnode, "XMLInternalDocument"))
}
\arguments{
  \item{gnode}{the SVG node whose bounding box is to be computed}
  \item{doc}{the XML document in which the SVG node an element}
}
\value{
 Currently, this returns either a Circle object (a numeric vector with 3 elements giving
 the x and y coordinates of the center and the radius), a Polyline,
 Polygon, Line, VerticalLine, HorizontalLine object or a   
 or a 2 x 2 matrix. The columns correspond to the horizontal (x) and vertical (y)
 coordiantes and the rows give the minimum and maximum of the
 these dimensions for the rectangle. 
}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{addLink}}
}
\examples{
 doc =  svgPlot({
            plot(c(1:13, 13:1), pch = 0:25)
            abline(h = c(2, 5, 9) + .5, col = 'lightgray', lty = 3)
            lines(1:13, col = 'lightgray', lty = 2)
          })

      # 2 "plot regions" corresponding to the points and 
  rr = getPlotRegionNodes(doc)

  getBoundingBox(rr[[1]])

  xmlSize(rr[[1]])
  groupByPch(xmlChildren(rr[[1]]), 0:25)
  xmlSize(rr[[1]])


  getBoundingBox(rr[[1]][[1]])

  table(xmlSApply(rr[[1]], function(x) class(getBoundingBox(x))))
  table(xmlSApply(rr[[2]], function(x) class(getBoundingBox(x))))
}
\keyword{iplot}
\keyword{aplot}
\keyword{dynamic}
\keyword{device}

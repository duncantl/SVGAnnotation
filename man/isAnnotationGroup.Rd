\name{isAnnotationGroup}
\alias{isDecorationGroup}
\Rdversion{1.1}
\alias{isAnnotationGroup}
\alias{isDecorationGroup}
\title{Determine if an SVG element is made up of sub-elements positioned
     inside another region}
\description{
  When we decorate an R plot with calls to functions
  such as \code{\link[graphics]{lines}},
  \code{\link[graphics]{abline}},  
  \code{\link[graphics]{text}},
  \code{\link[graphics]{symbols}}
  and so on, these appear in the resulting SVG in a separate
  group (\code{<g>}) element. This function allows us to
  determine if such a group is made up of SVG child elements
  that are physically/geographically located within
  another region's area, i.e. located  within
  the bounding box of the \code{parent} node.
  If they are, this is likely to be a collection of decorations.
}
\usage{
isAnnotationGroup(g, parent, bbox = getBoundingBox(parent), scalar = TRUE)
}
\arguments{
  \item{g}{an SVG element whose children are tested to determine
    which objects
    }
  \item{parent}{the parent SVG element of whom we are querying whether
    the elements in \code{g} are inside this element's bounding box}
  \item{bbox}{the bounding box object of the parent which can be
    specified without a parent if it has already been computed}
  \item{scalar}{a logical value indicating whether we want a single
    predicate value indicating whether all of the elements are inside
    the bounding box, or alternatively a vector with a logical value for
    each of the child nodes of \code{g} indicating which are and which
    aren't contained in \code{parent}'s bounding box.}
}
\value{
 A logical value. 
}
\author{
Duncan Temple Lang
}

\seealso{
  \code{\link{isInside}}
  \code{\link{getBoundingBox}}  
}
\examples{

doc = svgPlot({
           plot(1:10, pch = '+', bg = c("red"), fg = "blue")
           abline(v = c(3, 7), col = 'blue', lty = 3)
           abline(h = c(3, 7, 9), col = 'lightgray', lty = 2)           
        })

r = getPlotRegionNodes(doc)
length(r)  # 1 and they are just the lines.

SVGAnnotation:::isAnnotationGroup(r[[2]], r[[1]])
}
\keyword{programming}
\keyword{dplot}



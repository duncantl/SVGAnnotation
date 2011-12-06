\name{pathToCircle}
\Rdversion{1.1}
\alias{pathToCircle}
\title{Convert generic path SVG node to a circle}
\description{
  R's \code{\link[grDevices]{svg}} function,
  based on libcairo, draws circles using
  an SVG (cubic) Bezier path rather than
  the \code{<circle>} element.
  This function allows us to change such a node to the
  \code{<circle>} representation.
}
\usage{
pathToCircle(path, box = getBoundingBox(path), force = FALSE, addTypes = TRUE)
}
\arguments{
  \item{path}{the \code{<path>} node that is to be turned into a
    \code{<circle>} node.}
  \item{box}{the bounding box of the input/source node being converted. This should be of class Circle.}
  \item{force}{a logical value. If the input node does not correspond to
    a circle, this controls whether we force the conversion anyway.}
  \item{addTypes}{a logical value indicating whether to add a
    \code{type} attribute to the new node with a value of 'plot-point',
      making it easier to identify in subsequent computations. }
}

\value{
The new node or the existing one if it is not suitable for change!
}
\author{Duncan Temple Lang}


\seealso{
  \code{\link{svgPlot}}
}
\examples{
doc = svgPlot(plot(1:10))
rr = getPlotRegionNodes(doc)
names(rr[[1]])

pathToCircle(rr[[1]][[1]])

names(rr[[1]])


as(getBoundingBox(rr[[1]]), "Circle")

pathToCircle(rr[[1]]) # silently leaves things unchanged
pathToCircle(rr[[1]], force = TRUE) # silently leaves things unchanged
}

\keyword{programming}
\keyword{dplot}

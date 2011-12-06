\name{getTopContainer}
\alias{getTopContainer}
\alias{getTopG}
\title{Get the top-level g node in an SVG document}
\description{
  This function is a marginally convenient way to fetch the
  top-most \code{g} node in the SVG document that contains all
  the rendered components. This is the element with an id attribute
  whose value is \code{'surface0'} and whose sub-nodes are typically
  of interest. The other part of the document is the \code{defs}
  which contain definitions that are used by sub-nodes of this \code{g}
  node.
}
\usage{
getTopContainer(doc)
}
\arguments{
  \item{doc}{the parsed SVG/XML document}
}
\value{
 The \code{XMLInternalNode}.
}
\author{
Duncan Temple Lang
}
\seealso{
 \code{\link{svgPlot}}
}
\examples{
  doc = svgPlot(plot(1:10))
  getTopContainer(doc)
  getTopG(doc)
}
\keyword{graphics}
\keyword{programming}



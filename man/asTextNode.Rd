\name{asTextNode}
\Rdversion{1.1}
\alias{asTextNode}
\title{Change cairo representation of text to an explicit text element}
\description{
  Text in an SVG document created by R's graphics system(s)
  and libcairo is represented by a general \code{<g>}
  element. It is sometimes useful to turn these into
  
}
\usage{
asTextNode(node, text, replace = TRUE, addChildren = FALSE, ...,
            .attrs = list(...), vertical = NA)
}

\arguments{
  \item{node}{the original node}
  \item{text}{the text to display in the new \code{<text>} element.}
  \item{replace}{a logical value that controls whether we replace the original node }
  \item{addChildren}{a logical value}
  \item{\dots}{name = value pairs of attributes for the new SVG text node}
  \item{.attrs}{a named list or character vector to use as attributes
    for the new SVG text node.}
  \item{vertical}{a logical value indicating whether the text is known
  to be vertical (\code{TRUE}) or  horizontal (\code{FALSE}). Other
   rotations should be specified in the attributes.}
}
\value{

}
\references{

}
\author{
Duncan Temple Lang
}


\seealso{
  \code{\link{svgPlot}}
  \code{\link[grDevices]{svg}}
  \code{\link[XML]{newXMLNode}}
}
\examples{

  doc = svgPlot({
            plot(mpg ~ wt, mtcars, main = "Motor Trend Cars")
          })

  ax = getAxesLabelNodes(doc)
print(ax[[1]])
  newTitle = asTextNode(ax[[1]], "Motor Trend Cars")
  xmlAttrs(newTitle) = c('font-size' = 20)
  newXMLNode("set", attrs = c(attributeName = "text", values = "A, B, C", begin = "1s", dur = "4s"),
               parent = newTitle)
 
}
\keyword{programming}
\keyword{aplot}
\keyword{dplot}

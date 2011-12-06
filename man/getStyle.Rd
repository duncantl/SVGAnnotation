\name{getStyle}
\alias{getStyle}
\alias{setStyle}
\alias{modifyStyle}
\title{Manipulate the style attribute of an SVG node}
\description{
 These functions allow us to query and set and reorganize
 the value of a style attribute of an SVG node.
  We use these to determine the vector of CSS-based style
 settings and to set a new value.

 \code{modifyStyle} is used to  merge new values for particular
 characteristics in an existing style.

 These functions are provided for use in building
 higher-level functionality such as tooltips, animation, and other
  effects in SVG plots.
}
\usage{
getStyle(node)
setStyle(node, ..., .style = structure(unlist(list(...)),
          names = names(list(...))))
modifyStyle(style, ..., .vals = list(...))
}
\arguments{
  \item{node}{the SVG node whose style is to be manipulated}
  \item{style}{the style to be modified. This can be a character vector or an XML node.}
  \item{...}{name = value pairs giving the attributes and their values for the
      style}
  \item{.style}{ a named vector containing the name - value pairs
    as if they were typed via \dots.}
  \item{.vals}{the name-value settings for the styles given as a single
     list rather than  individually via the \dots.}
}
\value{
  \code{getStyle} returns a named character vector.
  \code{setStyle} returns the updated node.
  \code{modifyStyle} returns the updated object it was given.
}
\author{Duncan Temple Lang}

\seealso{
 \code{\link{convertCSSStylesToSVG}}
}
\examples{
 doc = svgPlot(plot(1:10, col = c("red", "blue")))

 pts = getPlotPoints(doc)

 sty = getStyle(pts[[1]])
 sty1 = modifyStyle(sty, fill = "green", 'stroke-width' = 2)
 
}
\keyword{iplot}
\keyword{aplot}
\keyword{dynamic}
\keyword{device}

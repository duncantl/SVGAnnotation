\name{getCSS}
\Rdversion{1.1}
\alias{getCSS}
\alias{getJavaScript}
\alias{getECMAScript}
\alias{dim,SVGDocument-method}
\alias{dim.SVGDocument}
\alias{getLatticeObject}
\alias{getSVGNodeTypes}
\alias{getUSR}
\alias{getRCommand}
\title{Query the SVG document for information}
\description{
  These functions allow us to query the SVG plot
  for information such as the dimensions of the
  SVG canvas/view-box, the CSS or JavaScript code
  it currently contains or refers to,
  the lattice/trellis object that was used to create the plot,
  the high-level description of the elements in the SVG
  corresponding to lattice plot componets (\code{getSVGNodeTypes}).

  \code{getUSR} returns the value of the \code{usr} attribute
  on the \code{r:display} element. This value was retrieved
  from the device after the expression in \code{cmd}
  is evaluated. This only makes sense when there is a single plot.

  \code{getRCommand} returns the R command that were used to create the
  initial plot with \code{\link{svgPlot}}.

}
\usage{
getCSS(doc, asNodes = FALSE)
getECMAScript(doc, asNodes = FALSE)
getJavaScript(doc, asNodes = FALSE)
\method{dim}{SVGDocument}(x)
getLatticeObject(doc, asNodes = FALSE)
getSVGNodeTypes(doc)
getRCommand(doc)
}
\arguments{
  \item{doc,x}{the XML/SVG document as a tree}
  \item{asNodes}{a logical value indicating whether to return the
    XML/SVG nodes corresponding to the content rather than the content
    itself. This can be useful if we want to modify the nodes or even
    remove them.}
}
\value{
  \code{getCSS} and \code{getECMAScript} return
  a character vector containing the content in the SVG
  document of that type.

  The \code{dim} method returns a numeric vector (not an integer vector
  as \code{dim} normally would for, e.g., a matrix or data frame or
  array).  The names of the elements give the units. (In the future This might be made
  an explicit attribute rather than using the names attribute.)


  The \code{getSVGNodeTypes} function returns a character vector.
}

\references{
\url{http://www.w3.org/TR/SVG11/}
}
\author{
Duncan Temple Lang
}

\seealso{
  \code{\link{svgPlot}}
  \code{\link{addECMAScripts}}
  \code{\link{addCSS}}  
}
\examples{
  doc = svgPlot(plot(1:10))
  dim(doc)

  addCSS(doc)
  addCSS(doc, I("class {\n  fill: red;\n  stroke: blue;}"))
  addCSS(doc, "http://www.omegahat.org/OmegaTech.css", FALSE)

  f = system.file("JavaScript", "link.js", package = "SVGAnnotation")
  addECMAScripts(doc, f)
  addECMAScripts(doc, f, insert = TRUE)


  getCSS(doc)
  getECMAScript(doc)

  data(mtcars)
  library(lattice)
  pp = svgPlot(xyplot( mpg ~ wt | am + cyl, mtcars,
                      group = carb,
                      auto.key = list(columns = 4, space ="right")))
  getSVGNodeTypes(pp)
}
\keyword{programming}
\keyword{aplot}
\keyword{iplot}


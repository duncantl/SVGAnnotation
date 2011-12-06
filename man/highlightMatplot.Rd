\name{highlightMatplot}
\alias{highlightMatplot}
\alias{getMatplotSeries}
\title{Make a matplot interactive, highlighting a series when mouse over}
\description{
  This function is for making an matrix plot or parallel coordinates plot
  interactive by allowing the viewer to mouse over a series to highlight
  it by enlarging its width and potentially changing its color.
  This helps to identify individual series when they are heavily
  overlayed and "tangled".

  The function \code{getMatplotSeries} is a lower-level function that
  can be used to find the SVG nodes that correspond to the polylines
  that draw each series.  Each series will have a single node.
  One can fetch these nodes with this function and then, e.g., add
  tooltips,  associate them with legend items, etc.
}
\usage{
highlightMatplot(doc, color = "null", factor = 3, series = getMatplotSeries(doc),
                 ids = seq(along = series),
                 js = system.file("JavaScript", "imatplot.js", package = "SVGAnnotation"))
}
\arguments{
  \item{doc}{the SVG document (or the name of a file which contains the SVG document)}
  \item{color}{a string giving a color (in JavaScript notation) that is
     used when highlighting a series to change the color of the line to
     that color.}
  \item{factor}{a scaling factor as a number that is used to change the
    width of the line for the highlighted series. One can use a value of
  1.0 to specify no change in width.}
  \item{series}{a list of XML nodes identifying the poly-lines for each
    series. Each series has a single node. One can fetch these from the
    plot using the function \code{getMatplotSeries}.}
  \item{ids}{a vector of values that are used as id attributes on the
    XML nodes in \code{series}.}
  \item{js}{the JavaScript code to add to the plot}
}
\value{
 The updated SVG document.
}
\references{}
\author{
  Duncan Temple Lang
}
\seealso{
  \code{\link{matplot}}
}
\examples{
 if(require(MASS)) {
   doc = svgPlot(parcoord(mtcars))
   highlightMatplot(doc, "red")
   saveXML(doc, "imtcars.svg")
 }
}
\keyword{graphics}
\keyword{iplot}

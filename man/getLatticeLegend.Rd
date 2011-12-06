\name{getLatticeLegendNodes}
\alias{getLatticeLegendNodes}
\title{Get the nodes corresponding to the legend in a lattice plot}
\description{
  This function uses heuristics to find the SVG nodes corresponding
  to the legend generated via the \code{auto.key}  parameter
  in a lattice plot. One can then use this  to annotate
  the SVG, e.g. put links or mouseover handlers on the labels.
}
\usage{
getLatticeLegendNodes(doc, plotRegions = getPlotRegionNodes(doc), which = NA)
}
\arguments{
  \item{doc}{the parsed SVG document, i.e. an object of class \code{XMLInternalDocumet}}
  \item{plotRegions}{typically not specified, but this allows the caller
    to identify the plot region nodes.}
  \item{which}{if specified, an integer identifying which legend is of
    interest. This is for the case where there is more than one
    legend/key in the plot, i.e. via the \code{legend} argument.
    }
     
}

\value{
 A list of \code{XMLInternalNode} objects.
}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{svgPlot}}
  \code{\link[XML]{xmlParse}}
  the lattice package.
}
\examples{
 library(lattice)
 d = data.frame(x = runif(100), y = runif(100),
                type = sample(c("A", "B", "C"), 100, replace = TRUE),
                g = sample(c("w", "x", "y", "z"), 100, replace = TRUE))
 doc = svgPlot(xyplot(y ~ x | type, d, groups = g, auto.key = TRUE))
 nodes = getLatticeLegendNodes(doc)
 length(nodes)
}
\keyword{hplot}
\keyword{iplot}

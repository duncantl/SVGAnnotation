\name{xmlPlot}
\alias{xmlPlot}
\alias{histogramPlot}
\alias{scatterPlot}
\alias{pairsPlot}
\title{Constructor functions for different types of plots in SVG}
\description{
  
 These functions allow one to read an SVG file containing an R plot into
 R and then process it in different ways.  The different functions are
 used to identify the type of the plot. This can facilitate working with
 the plot where the layout is important to know.  At present, this is
 not essential.  In fact, one should use \code{\link{svgPlot}} to create
 the SVG plot and return the resulting SVG/XML tree.
 
}
\usage{
xmlPlot(file, class = character())
}
\arguments{
  \item{file}{the name of the SVG file from which to read the plot} 
  \item{class}{the name of the class for the resulting parsed XML document.
   This allows one to identify the plot as, for example,  a histogram, scatter plot,
  }
}
\value{
  A parsed XML document with the relevant class attribute.
}
\author{Duncan Temple Lang} 
\seealso{
  \code{\link{svgPlot}}  
}
\examples{

}
\keyword{iplot}
\keyword{aplot}
\keyword{dynamic}
\keyword{device}

\name{XMLPlot-class}
\docType{class}
\alias{XMLPlot-class}
\alias{GRZXMLPlot-class}
\alias{LatticeXMLPlot-class}
\alias{BarchartXMLPlot-class}
\alias{DensityXMLPlot-class}
\alias{HistogramXMLPlot-class}
\alias{PairsXMLPlot-class}
\alias{ScatterXMLPlot-class}
\alias{TimeSeriesXMLPlot-class}
\alias{XYXMLPlot-class}


\title{Classes for representing different types of plots in SVG}
\description{
 These classes are intended to provide information about the content of an 
 SVG plot in R after we read the XML tree into R.
  Many operations can be done on a plot with no knowledge of what type
  of plot it is, e.g. a scatter plot or a box plot.  However, when 
  the processing depends on the type of the plot, then we can use these
 classes to identify the nature of the plot and to identify appropriate
 methods. For example, a histogram is made up of rectangles and tooltips
 are very different for these than for points in a scatter plot.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
	 \describe{
    \item{\code{.S3Class}:}{Basically, we use these S3 classes as labels identifying
    the type of plot. We may add information such as the number of panels, the conditioning variables, etc.
      in a lattice plot.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{XMLInternalDocument}"}, directly.
Class \code{"\linkS4class{XMLAbstractDocument}"}, by class "XMLInternalDocument", distance 2.
Class \code{"\linkS4class{oldClass}"}, by class "XMLInternalDocument", distance 3.
}
\section{Methods}{
No methods defined with class "XMLPlot" in the signature.
}
\author{Duncan Temple Lang}

\seealso{
 \code{\link{xmlPlot}} and related constructors.
}
\examples{
showClass("XMLPlot")
}
\keyword{classes}

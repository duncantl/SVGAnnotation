\name{groupByPch}
\Rdversion{1.1}
\alias{groupByPch}
\alias{numPathElementsPerPCH}

\title{Group multiple SVG paths corresponding to same plotting character}
\description{

  Typically, there is one SVG \code{<path>} element for each observation
  displayed in a plotting region. However, there are 10 different
  plotting   characters  in R that are made up multiple line segments
  and so give rise to multiple \code{<path>} elements in the resulting
  SVG output. To work with these more easily and sanely,
  it is convenient to convert the multiple path objects
  into a single group object for each such "point" in the display.
  This function does precisely that.
  
  Note that is very difficult for us in this package to be able to infer
  the plotting character for a point except for in the most trivial of
  cases. That is why we have made this function available
  so that those who have contextual knowledge of the plot can perform
  this regrouping.

  We collect the nodes related to the same observation and
  put them into a new \code{<g>} node and replace these nodes
  with that in the orginal parent node.
  We also add both a \code{pch} attribute to the \code{<g>} node
  which identifies to which pch value the new \code{<g>} corresponds
  and the \code{type = 'plot-point'} attribute-value pair.
  
  The number of line segments/path elements for each plotting character
  is available in the variable \code{numPathElementsPerPCH}.
}
\usage{
groupByPch(nodes, pch)
}
\arguments{
  \item{nodes}{the set of nodes corresponding to the observations,
    including some \code{<path>} elements that correspond to the same  observation.}
  \item{pch}{the vector of values specified via the \code{pch} command
       to generate the plot in R that corresponds to these nodes,
       i.e. there may be different commands and different pch vectors
       for different subplots. }
}
\value{
  The main result is the reorganization of the nodes.
  The return value is an integer vector that provides the grouping of
  the original nodes. The names indicate the plotting character
  from which the original node came.
}

\author{Duncan Temple Lang}

\seealso{
  \code{\link{getPlotRegionNodes}}

  See \url{http://www.omegahat.org/SVGAnnotation/pch.svg}
  for an example of how we collapsed the plotting characters
  into groups. The code to create this is below.
}
\examples{

pch = c(7, 8, 10)
pp = svgPlot(plot(1:10, pch = pch))

rr = getPlotRegionNodes(pp)[[1]]
xmlSize(rr)
pts = getPlotPoints(pp)
length(pts)

groupByPch(pts, pch)
xmlSize(rr)
# Note that pts is irrelevant now.
# We need to fetch it again
pts = getPlotPoints(pp)


###########


doc = svgPlot(plot(1:26, pch = 0:25, bg = c("red")))
rr = getPlotRegionNodes(doc)[[1]]

v = groupByPch(xmlChildren(rr), 0:25)

  # now how many are remaining?
xmlSize(rr)

  # size of all the nodes. 0's correspond to path elements.
xmlSApply(rr, xmlSize)
  # how many are in each of the g's
sapply(rr[names(rr) == "g"], xmlSize)

  # check the pch attribute
sapply(rr[names(rr) == "g"], xmlAttrs)

saveXML(doc, "pch.svg")


################

pch = c(8,0, 3)
doc = svgPlot(plot(1:10, pch = pch))

rr = getPlotRegionNodes(doc)[[1]]
pts = getPlotPoints(doc)

xmlSize(rr)

groupByPch(pts, pch)
xmlSize(rr)
xmlSApply(rr, xmlSize)
sapply(rr[names(rr) == "g"], xmlSize)
names(rr)

sapply(rr[names(rr) == "g"], xmlAttrs)
xmlSApply(rr, function(x) "type" \%in\% names(xmlAttrs(x)))

xmlSApply(rr, xmlGetAttr, "pch")
}
\keyword{programming}
\keyword{dplot}

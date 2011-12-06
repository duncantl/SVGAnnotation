\name{getAxesLabelNodes}
\alias{getAxesLabelNodes}
\title{Retrieve the XML nodes representing the title and axes label nodes}
\description{
 This function examines the XML document and determines the nodes
 which represent the text annotating the main title and the
 X and Y axes for each sub-plot within the R graphic.
 The function can distinguish between SVG for lattice and traditional (grz)
 plots.
 
 At present, the function does not identify which nodes correspond to which 
 axes, title, etc.  We can work this out via the getBoundingBox function
 and compare these to the viewbox of the entire SVG display (\code{getViewBox})
 and within each plot region (getPlotRects)
}
\usage{
getAxesLabelNodes(doc, addTypes = TRUE)
}
\arguments{
  \item{doc}{the parsed SVG document}
  \item{addTypes}{a logical value indicating whether to add a
    \code{type} attribute to the new node with a value of 'plot-point',
      making it easier to identify in subsequent computations. }  
}
\value{
  This attempts to find the text nodes associated with the relevant plot region.
  The result is currently the list of \code{<g>} nodes which contain the
  \code{<use>} elements.
}
\author{Duncan Temple Lang}

\examples{
 doc = svgPlot({
                 opar = par(no.readonly = TRUE)
 
                 par(mfrow  = c(3, 2))
                 replicate(6, plot(density(rnorm(100))))
               })

 ax = getAxesLabelNodes(doc)

   # one for each plot.
 length(ax) == 6

  # Each element has the main title, x axes and y axes labels.
 xmlSize(ax[[1]]) == 3


  ###########
 svg("inset_layout.svg")
  par(mfrow = c(1, 1))
   m = matrix(1, 10, 10)
   m[2:5, 3:8] = 2
   layout(m)
   
   x = rnorm(1000)
   plot(x, xlab = "observation number", ylab = "N(0, 1)")
   plot(density(x))
  dev.off()

 doc = xmlParse("inset_layout.svg")
 ax = getAxesLabelNodes(doc)

   # We have 2 elements in ax. The first is for plot 1 and has
   # only X and Y labels, no title.
   #
   # The second plot has 3 <g> elements within it.

   ##################

 library(lattice)  
    x = runif(100, 5, 20)
    y = 4 + 3 * x + rnorm(length(x))

  svg('xyplot.svg')
  xyplot(y ~ x)
  dev.off()

  doc = xmlParse("xyplot.svg")
  ax = getAxesLabelNodes(doc)
 
    # ax has 2 elements - X and Y axes.
  print(ax)
  getBoundingBox(ax[[2]])
  # When run via R CMD check, this causes problems.
  # but  not interactively!
  #lapply(ax, getBoundingBox)
}
\seealso{
 \code{\link{addAxesLinks}}
 \code{\link{addToolTips}}
}
\keyword{iplot}
\keyword{aplot}
\keyword{dynamic}
\keyword{device}

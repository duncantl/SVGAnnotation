\name{svgPlot}
\alias{svgPlot}
\title{Wrapper for the svg() function in the grDevices to create a plot}
\description{
  
 This function acts as a front-end/wrapper function
 for the corresponding function in the grDevices package.
 The intent is that the caller both opens the device
 and provides the commands to generate the entire plot within
 this call.
 This function creates the graphics device in the same way 
 the grDevices (or CairoSVG on windows) function does.
 It then evaluates \code{cmd} in order to generate the plot.
 It then closes the graphics device and reads the SVG file
 back into R using the \code{XML} package.
 It adds the text of the R command \code{cmd}
 to the SVG file and writes that out again.
 If \code{addInfo} is \code{TRUE} (and if the plot is a lattice plot)
 it adds a great deal of meta-information to the SVG elements,
 including details about the lattice plot
 and also attempting to identify the type of each top-level element
 within the top-most \code{<g>} element in the SVG.

 One can have the SVG document returned as a parsed XML document.
 This allows us to ignore the creation of the file and "pretend" that
 the SVG content is created directly in memory.
}
\usage{
svgPlot(cmd, filename = paste(tempfile(), "svg", sep = "."), ...,
        asXML = missing(filename),
        print = inherits(ans, "trellis"),
        removeFile = asXML, addInfo = TRUE)
}
\arguments{
  \item{filename}{the name of the file to which the plot is written}
  \item{cmd}{the R expression generating the plot}
  \item{\dots}{any additional arguments to the grDevices \code{svg} function,
    e.g. the dimensions of the plot, point size, background, ...}
  \item{asXML}{a logical value which indicates that whether the
   SVG document should be parsed and returned.  The file is still
   created and left in place.
 }
 \item{print}{a logical value indicating whether we need to explicitly
   print the result of \code{cmd} to generate the plot. This is
   necessary for lattice plots.
 }
 \item{removeFile}{a logical value indicating whether to delete the
   file that was created by the \code{svg} device.
   If one wants the parsed XML document returne, there is typically no need
   to keep the file, so this option controls whether it is removed at
   the end of the call to this function.
 }
 \item{addInfo}{a logical value that controls whether
   meta-information is added to the SVG document that both describes the
   plot and also attempts to identify the top-level SVG elements in the
   display.
 }
% \item{doc}{the XML tree that is  the SVG display document}
}
\value{
  The value of \code{filename}. 
}

\author{Duncan Temple Lang}
\seealso{
 \code{\link[grDevices]{svg}}
}
\examples{
  doc = svgPlot(plot(1:10))
  svgPlot({ 
             plot(rnorm(100), col = c("red", "green"))
             abline(v = 0, lwd = 2, lty = 3)
           }, "tmp1.svg")


  data(mtcars)
  library(lattice)
  pp = svgPlot(xyplot( mpg ~ wt | am + cyl, mtcars, group = carb,
                        auto.key = list(columns = 4, space ="right")))
  getSVGNodeTypes(pp)

  dim(pp)
  x = getLatticeObject(pp)

   # with a user-specified key.
  o = svgPlot(xyplot( mpg ~ wt | am + cyl, mtcars, group = carb,
                      key = simpleKey(text = c("A", "B", "C"),
                                      points = FALSE,
                                      columns = 3)))
}
\keyword{iplot}
\keyword{dynamic}
\keyword{device}


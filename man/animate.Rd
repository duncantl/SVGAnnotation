\name{animate}
\alias{animate}
\title{A simple-minded illustration of  animating a scatter plot}
\description{
 This function provides an illustration of animating a scatter plot
 in a simple-minded manner.
 The idea is that we take an existing  scatter plot of variables X and Y 
 illustrating observations at a particular time period.
 Then, we move the points in that scatter plot to their locations 
 at the next time period and then again for the next time period.
 When we start a new time period, the points change color.

  If you use plotting characters in the original plot
  that have multiple segments or paths, you will need
  to call \code{\link{groupByPch}} in order to
  move these segments into  their own groups.
  This results in the correct number of SVG elements
  corresponding to the observations and then the animation will work fine.
  
}
\usage{
animate(file, data,
        which = if(ncol(data) > 2)
                  data[,3]
                else
                  stop("you must specify which to identify the time steps"),
        xlim = NA, ylim = NA,
        interval = 2,
        begin = interval,
        dur = "10s",
        colors = character(),   
        radii = list(),
        digits = 4, 
        doc = xmlPlot(file),
        plotRegion = getPlotRegionNodes(doc)[[1]],
        points = getPlotPoints(plotRegion),
        dropFirst = TRUE,
        labels = character(), labelInfo = list(pos = "center"),
        hook = NULL, ..., .args,
        addTypes = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{the SVG file containing the original scatter plot}
  \item{data}{the data to be plotted. This is a data frame or matrix
      containing the X and Y values for all time periods of interest.}
  \item{which}{a vector that identifies the "time" variable.
    This is used to group the observations so we move all the points
    within a given group at the same time. This is likely to be a factor
    such as day of week, or month, or year}
  \item{xlim}{the range in data coordinates for the horizontal axis. This should  be
    the same as  used for the original scatter plot.
    It is vitally important that these values (and the corresponding
    \code{ylim})
    values give the actual range of the X range of the entire data
    region  not just the xlim used in creating the initial plot.
    In general, we want the value of \code{par("usr")} for the
    particular plotting region.
    Since \code{svgPlot} inserts this into the SVG document, we
    are better off using the default value.
  }
  \item{ylim}{the range in data coordinates for the vertical axis.}
  \item{begin}{the number of seconds before the animation starts. The
    original plot will be displayed (without motion) for this period}
  \item{interval}{the number of seconds each step of the animation
    lasts.  One can either specify this or the total duration of the
    entire animation, but not both.}
  \item{colors}{a  vector of colors to use for the points in each time period.
    This should be the same length as the unique values in \code{which}.}
  \item{dur}{how long the animation should last. 
      This time is divided evenly between the different time periods.
      This defaults to seconds.  Instead of giving this total amount of
      time, one can use \code{interval} to specify the amount of time
      for each step of the animation.
    }
  \item{digits}{the number of digits to write for transformed (x, y) pairs written to the SVG file.}
  \item{doc}{the \code{XMLInternalDocument} created by parsing the contents of \code{file}.
    This can be passed directly if the SVG plot has already been parsed.
   This allows one to read the document and process it before adding the animation.}
  \item{points}{the list of SVG nodes representing the points in the original
    scatter plot.}
  \item{plotRegion}{the SVG node/element (\code{<g>}) corresponding to the plotting
    region in which the animation is occurring. }
  \item{dropFirst}{either a logical value or a value of the level in
    the\code{which} vector. This is allows the caller to have the
    function drop the observations in \code{data} that have already
    been plotted. These correspond to the first time point.
    We animate the rest of the points and this saves the caller
    having to perform the subset. This is especially
    useful as this allows the function to compute the \code{xlim}
    and \code{ylim} values on the entire data set and then drop
    the points that have already been plotted.}
  \item{labels}{a character vector whose values will be displayed
    sequentially at the start of each step of the animation in the
    middle of the plot in light gray color.}
  \item{labelInfo}{information controlling the location and appearance
  of the animated labels displayed in the plot that illustrate the passage of time and 
   the current time.  This should be a list. The pos element controls
  the location. This can be a numeric vector of length 2 giving the x,y coordinates
  at which the text will be placed; or a character vector with values "center" (or "centre"), 
   "top", "bottom", "right", "left".  The other elements in the list are used as attributes for the XML text element and so can control the font, its size, color, and several other attributes.}
  \item{radii}{a list or a character vector naming a column in
    \code{data}. This is a list of values with one element
    for each observation in the original plot.
    Each such element is a numeric vector with as many elements as there
    are stages in the animation, excluding the first.
    These values are used to multiply the radius of the observation's
    circle on the display at the corresponding stage of the animation.
    Note that they scale the original radius; they do not cumulate.
  }
  \item{hook}{a callback function that can be specified by the caller
    which will be invoked for each observation and for each step in the
    animation as we are creating the animation in R. (It is not called
    during the viewing of the animation, just in the preparation!)
    This can be used to adjust or add to the animation, e.g. put in
    trail lines, update text, synchronize with another plot.
    The function is called with the SVG element representing the
    observation/point on the plot, a list giving the details about the
    animation (the id, when the step begins, its duration, the index of
    the observation and the step in the animation) and any additional
    arguments passed via \code{\dots} or \code{.args}.
  }
  \item{\dots}{additional arguments that are passed to the calls to
    \code{hook}}
  \item{.args}{additional arguments that are passed to the calls to
    \code{hook} but which are specified as a list to avoid conflicts
    with names in this function.}
  \item{addTypes}{a logical value which if \code{TRUE} causes the function
  to add a \code{type} attribute to the new circle SVG elements it creates that
  identifies each of them as a 'plot-point'.}
}

\value{
  A list with two elements.
  \item{circles}{a list of the newly created or updated nodes
    corresponding to the observations in the plot.}
  \item{details}{a list with information about the animation such as the
    starting points of the different stages/steps, their duration, their
    names, the interval (as a number) and the starting offset. The first
  three elements are character vectors; the last two are numbers.}
}
\references{
\url{http://www.w3.org/TR/SVG11/animate.html}  
}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{svgPlot}}
  \code{\link[grDevices]{svg}}
  \code{\link{groupByPch}}
}
\examples{

#
# 4 points
#  1 going along a 45 degree line
#  one going 
#
#

p1 = matrix(c( 2, 2, 2,
               3, 3, 3,
               4, 4, 4,
               5, 5, 5, 
               6, 6, 6), 5, , byrow = TRUE)

p2 = matrix(c( 12, 2, 1,
               10, 3, 1.2,
                8, 4, 1.4,
                6, 5, 1.5, 
                4, 6, 3), 5, , byrow = TRUE)


p3 = cbind(seq(4, 12, length = 5),
           rep(6, 5),
           c(1.5, 3, 5, 3, 1.5))

D = data.frame(rbind(p1, p2, p3))

names(D) = c("x", "y", "radius")
D$time = rep(1:5, 3)
D$id = rep(1:3, each = 5)
D = D[order(D$time),]


if(FALSE) {
plot(0, xlim = range(D$x), ylim = range(D$y))
by(D, D$id, function(x){
             col = c("red", "green", "blue")[x[,"id"]]
             points(x[,1], x[,2], col = col)
             text(x[,1] + .1, x[,2] + .1, 1:nrow(x), col = col)             
             lines(x[,1:2], col = col[1])
           })
}

#########################################

library(SVGAnnotation)

   # We specify the xlim and ylim to ensure the other
   # points in time will
pp = svgPlot({plot(y ~ x, subset(D, time == 1),
                   pch = 21,
                   col = c("red", "green", "blue"),
                   bg = c("red", "green", "blue"),
                   xlim = range(D$x), ylim = range(D$y)
                  )
                    # Add horizontal and vertical lines at suitable positions.
               abline (h = c(2, 4, 6), col = "lightgray", lty = 3)
               abline (v = c(2, 7, 12), col = "lightgray", lty = 2)              
              })

animate(pp, D, "time", dropFirst = TRUE,
         labels = seq(2005, length = 5),
           begin = 1, radii = "radius")

saveXML(pp, "gm1.svg")



###############
# This illustrates how we use groupByPch() when we use plotting
# symbols that correspond to multiple SVG elements.

pch = c(7, 8, 10)
pp = svgPlot({plot(y ~ x, subset(D, time == 1), pch = pch, 
                   col = c("red", "green", "blue"),
                   bg = c("red", "green", "blue"),
                   xlim = range(D$x), ylim = range(D$y))
               with(D[D$time == 1,], points(x, y, col = c("red", "green", "blue")))
               with(D[D$time != 1,], points(x, y, col = 'lightgray'))              
               abline (h = c(2, 4, 6), col = "lightgray", lty = 3)
               abline (v = c(2, 7, 12), col = "lightgray", lty = 2)              
              })

rr = getPlotRegionNodes(pp)[[1]]
groupByPch(xmlChildren(rr), pch)

animate(pp, D, "time", dropFirst = TRUE, labels = seq(2005, length = 5),
          points = xmlChildren(rr)) 

saveXML(pp, "gm4.svg")


#######################

\dontrun{
load(url("http://www.omegahat.org/SVGAnnotation/tests/myTraffic.rda"))
svg("occ.xml")
plot(Flow1 ~ Occ1, traffic[1:288,],
      xlim = range(traffic$Occ1),
      ylim = range(traffic$Flow1),
      main = "Flow versus Occupancy")
abline(v = .2, col = "red")
dev.off()

     # Now we can animate
z = animate("occ.xml", traffic[-(1:288), c("Occ1", "Flow1")],
             weekdays(traffic$time[-(1:288)]),
             xlim = range(traffic$Occ1), ylim = range(traffic$Flow1),
             dur = "10s")
}
}
\keyword{iplot}
\keyword{aplot}
\keyword{dynamic}
\keyword{device}

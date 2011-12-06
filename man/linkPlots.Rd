\name{linkPlots}
\alias{linkPlots}
\title{Implement simple linking of observations across sub-plots
  within an R SVG graphic}
\description{
  This function implements a simple-minded linking mechanism
 of points within an R plot consisting of multipe sub-plots.
 The idea is that we use \code{\link[graphics]{pairs}} or 
 \code{par(mfrow = ...)} to create a plot in R with different
 variables from the same data set. Then we post process the SVG
 document and add handlers for mouseover events. When the 
 mouse is moved to be over a point in any of the plots, 
 all of the corresponding points in the other plots are 
 colored. When the mouse moves away from the point,
 the points in all the plots revert.

 We can relatively easily add facilities for 
  "persistence" whereby 
 one can click on a point and it would remain 
 colored when the mouse is moved away from the point.
}
\usage{
linkPlots(doc, col = "red",
          script = system.file("JavaScript", "link.js",
                                 package = "SVGAnnotation"), 
          insertJS = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the parsed SVG document.}
  \item{col}{the color to use for highlighting points under the mouse}
  \item{script}{the name of the JavaScript/ECMAscript file containing the code
    to implement the link event handlers.}
  \item{insertJS}{a logical value indicating whether we want to insert the contents
     of \code{insertJS} directly into the SVG document so that the resulting
     document will be self-contained. Alternatively, we can add a reference
     to the document. This allows us to then continue to edit that file
     and have the results be immediately available to the document.
  }
}
\details{
  For implementation reasons, we change the nature of the
 points in the SVG plots from a path to a regular circle.
}
\value{
  The updated SVG document as an XMLInternalDocument.
}
\references{}
\author{Duncan Temple Lang}

\seealso{
\code{\link[grDevices]{svg}}
}
\examples{

   svg("simple_link.svg")
   par(mfrow = c(2, 2))
   replicate(4, plot(1:10))
   dev.off()

   doc = xmlParse("simple_link.svg")
   linkPlots(doc)
   saveXML(doc, "links.svg")


      #####################


svg("mtcars_link.svg", 10, 10)
par(mfrow = c(1, 3), cex.lab = 2, cex.main = 2, cex = 1.4)
plot(mpg ~ cyl, mtcars)
plot(hp ~ wt, mtcars)
plot(carb ~ hp, mtcars)   
dev.off()

doc = xmlParse("mtcars_link.svg")
linkPlots(doc)
saveXML(doc, "mtcars_link.svg")


      #####################

svg("pairs_link.svg", 14, 10)
pairs(mtcars[,1:3], cex = 2)
dev.off()

doc = xmlParse("pairs_link.svg")
linkPlots(doc)
saveXML(doc, "pairs_link.svg")
}
\keyword{iplot}
\keyword{aplot}
\keyword{dynamic}
\keyword{device}

\name{addAxesLinks}
\alias{addAxesLinks}
\title{Add hyperlinks on some of the axes labels and/or plot title}
\description{
 This function allows one to associate target URLs with 
 axes labels and plot titles so that the viewer can click
 on the text and jump to the corresponding URL.

 If successful, this also adds the specified CSS content,
 but won't re-add it if it is already in the document.
}
\usage{
addAxesLinks(doc, links, nodes = getAxesLabelNodes(doc), 
              css = getDefaultSVGCSS(), style = "link")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the parsed XML document containing the SVG graphic}
  \item{links}{a character vector giving the links.
   At present, this is handled very simply with the elements
   corresponding to the order of the title and x and y axes
   assuming they are present. If there is no title, the first element goes with the 
    X axes label...}
  \item{nodes}{the nodes}
  \item{css}{a character vector giving the name of a CSS file
     which is added to the file. By default, this is added by reference.
     If you want to insert its contents, call \code{\link{addCSS}}
     explicitly.
     If this is given as the empty character vector or NA,
     no CSS is added.
   }
   \item{style}{a CSS style specification. This can be an inlined CSS
     specfication of the form "name: value; name: value; ..." such as
     "fill: red; stroke: blue;". 
     Alternatively, this can be a class name which refers to a class
     defined in (any of) the CSS content associated with this document.
     The function should be able to tell the difference between
     an inlined specification and a class name. However, we can
     explicitly  indicate that  it is an inline style by identifying
     it as of class \code{AsIs}  using the \code{\link[base]{I}}
     function, e.g.
     \code{style = I("fill: red; stroke: blue;")}.
     If we want to suppress any style/class information, we can specify
     this as \code{NA}.
   }
}
\value{
  The modified SVG document.
}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{addToolTips}}
}
\examples{
  doc = svgPlot(plot(1:10))

  addAxesLinks(doc, c("http://www.omegahat.org",
                      "http://www.r-project.org"),
                style = "fill: transparent; stroke: red;")
}
\keyword{iplot}
\keyword{aplot}
\keyword{dynamic}
\keyword{device}

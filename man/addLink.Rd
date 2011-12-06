\name{addLink}
\alias{addLink}
\title{Add a hyperlink to an SVG element}
\description{
  This makes an SVG element "clickable" via a hyperlink
  which will view the URL.
}
\usage{
addLink(n, url, addArea = NA, css = getDefaultSVGCSS(), style = "link", silent = FALSE)
}
\arguments{
  \item{n}{the XML/SVG node}
  \item{url}{the target of the link}
  \item{addArea}{a logical value indicating whether to create a new 
     rectangle node as the area the link will be active and put the existing contents of 
     \code{n} into the rectangle. The dimensions of the rectangle are calculated from the contents of 
     \code{n}.}
   \item{css}{the CSS content (file or string) to add to the document if
   style is not an inlined CSS style but the name of a CSS class.}
   \item{style}{a character string giving the name of the CSS class to
    use for any new SVG element that might be added to represent the
    event area, i.e. if a rectangle is added to "enclose" text or a
    phrase.
    This can be be either a class name or an inline CSS specification
    of the form "name: value; name: value; ...".
    To remove any ambiguity about whether it is a class name or inine
    specfication     of CSS values, one can use \code{\link[base]{I}}
    to indicate it is a collection of CSS values, e.g. \code{style =
      I("fill: red; stroke: orange;")}
  }
  \item{silent}{a logical controlling whether \code{\link{addCSS}} emits
    a warning if there is a directive to load the  CSS file is already
    in the target document.
   }
}
\value{
  The newly created link node, i.e. the \code{<a xlink:href="..">} element.
  The return value is typically not important as the node has been added to the document.
}
\references{
 The SVG specification \url{http://www.w3.org/Graphics/SVG/}
}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{addAxesLinks}}
}
\examples{
  doc = svgPlot(plot(1:10, main = "The SVGAnnotation Package"))
  nodes = getAxesLabelNodes(doc)
  addLink(nodes, c("http://ww.r-project.org",
                    "http://www.omegahat.org",
                    "http://www.omegahat.org/SVGAnnotation"))

  nodes = getPlotPoints(doc)
  addLink(nodes,
           paste("http://ww.r-project.org", seq(along = nodes), sep =""))
}
\keyword{iplot}
\keyword{hplot}

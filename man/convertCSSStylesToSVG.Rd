\name{convertCSSStylesToSVG}
\alias{convertCSSStylesToSVG}
\alias{convertCSSStylesToSVG,XMLInternalDocument-method}
\alias{convertCSSStylesToSVG,XMLInternalNode-method}
\alias{convertCSSStylesToSVG,XMLNodeSet-method}
\alias{convertCSSStylesToSVG,list-method}
\title{Convert a CSS-style attribute to separate SVG attributes}
\description{
  In SVG, one can represent style settings (e.g.
  stroke, stroke-width, fill and so on)
  using separate SVG attributes or as a single
  \code{style} attribute with a value that
  lists all the settings in CSS form, i.e.
  with ';'-separated  name: value pairs.
  So \code{<g stroke="red" stroke-width="3" fill="blue">}
  can also be written as
  \code{<g style="stroke: red; stroke-width: 3; fill: blue">}

  This function and its methods allow us to convert from
  the CSS form to the separate-attribute form.
  This can make processing the document and its elements
  easier in both R  and Javascript. It does mean that
  we cannot use a CSS file to specify and control the
  appearances of the nodes.

  This function and methods operate on a document, an individual node,
  a list of nodes, or an \code{XMLNodeSet} returned from
  \code{\link[XML]{getNodeSet}}.
}
\usage{
convertCSSStylesToSVG(nodes, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nodes}{an XML node (\code{XMLInternalNode}),
    a document (\code{XMLInternalDocument}), a list of
    \code{XMLInternalNode} objects, or an
    \code{XMLNodeSet} returned from \code{\link[XML]{getNodeSet}}.
   }
  \item{\dots}{additional parameters for the methods}
}
\value{
 The updated object with the modified internal nodes.
}
\references{
 \url{http://www.w3.org/TR/SVG11/styling.html}  
}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{svgPlot}}
  \code{\link{compressStyles}}  
}
\examples{
doc = svgPlot(plot(1:10, pch = c("red", "blue")), asXML =  TRUE)
nodes = getNodeSet(doc, "//*[@style]")
convertCSSStylesToSVG(nodes[[1]])

nodes = getNodeSet(doc, "//*[@style]")
convertCSSStylesToSVG(nodes)

doc = svgPlot(plot(1:10, pch = c("red", "blue")), asXML = TRUE)
convertCSSStylesToSVG(doc)
}
\keyword{hplot}

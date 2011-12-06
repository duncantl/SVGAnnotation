\name{compressStyles}
\alias{compressStyles}
\title{Replace repeated inline CSS style settings with a CSS document}
\description{
  This function takes an SVG document and extracts all the style
  attributes within the nodes and converts them to
  a set of CSS specifications and then replaces the
  style attribute with a class attribute which associates
  that node with the CSS declaration/rule.
  This can lead to a "significant" reduction in the size of the file
  (~ 30\%) by avoiding duplication.  It also allows one
  to easily change the appearance of all nodes with the same
  settings by modifying the centralized CSS rule.
  Furthermore, it allows somebody 
  to customize the appearance of a plot
  without using R commands and regenerating the plot.
  This can be useful if the person doesn't know R
  or does not have the original data.
}
\usage{
compressStyles(doc,
               classNames =
                       function(x)
                         paste("style", seq(along = x), sep = ""),
               threshold = NA, add = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the parsed SVG document from \code{\link{svgPlot}} or \code{\link[XML]{xmlParse}}.}
  \item{classNames}{names for the newly created styles which will be
    and which will be used as the values of the \code{class} attribute in
    the nodes}
  \item{threshold}{a number (or \code{NA}) that controls whether to
    bother  converting the style nodes into CSS entries.
    The idea is that we will only bother doing the conversion if
    there is a sufficient number of repeated style values.
    If all are different, we will have as many CSS rules as there are
    style nodes and so it may not be worth separating them.
    This parameter identifies the cut-off point at which
    we bother to create the CSS entries.  If this is \code{NA},
    we do the conversion. If this is a number between 0 and 1,
    we take this as a percentang and if the proportion 
    the number of unique styles divided by the total number of styles is
    less than this, we do the conversion.
    Alternatively, if \code{threshold} is an integer greater than 1,
    we do the conversion if the number of unique styles is greater than
    this value.
   }
   \item{add}{either a logical value or a character string.
         This controls whether or how the newly created CSS content is added
         to the document. If \code{add} is a character string, we use
	 this as the name of the CSS file and write the CSS content to
	 that and  add a reference to that CSS file in the SVG document.
	 If \code{add} is a logical value and \code{TRUE}, we insert the
	 newly generated CSS file directly into the SVG document.
	 If it is \code{FALSE}, we do not add the CSS content to the SVG
	 document but just return the new styles as a character vector
	 and allow the caller to use them in whatever she wants.
       }
}

\value{
  If \code{add} is \code{FALSE}, a named character vector giving the
  newly-created CSS styles.
  Otherwise, the updated \code{XMLInternalDocument} object is returned.
  If \code{add} is a character string, a new file is created with that
  name and contains the CSS definitions.
}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{svgPlot}}
  \code{\link{convertCSSStylesToSVG}}  
}
\examples{
 d = data.frame(x = runif(100), y = runif(100),
                type = sample(c("A", "B", "C"), 100, replace = TRUE))

 doc = svgPlot(plot(y ~ x, d, col = c("red", "green", "blue")[type]))
 compressStyles(doc)
}
\keyword{hplot}
\keyword{IO}

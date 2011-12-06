\name{addToolTips}
\alias{addToolTips}
\title{Add tooltips to SVG elements}
\description{
 This function and methods add tooltips to SVG elements
 of a plot that appear when the viewer places the 
 mouse over the element and leaves it there for a second or two.
 These tooltips can provide more information about the element
 such as the observation number of a point, its values
 for other variables, a description of the units of a variable
  for the axis label, or an overview of a plot from its 
  title element.
}
\usage{
addToolTips(file, text = seq(along = paths),
            which = seq(along = text), 
            doc = xmlPlot(file), save = FALSE,
            paths = getPlotPoints(doc), 
            elName = "title", addArea = NA,
            style = "tooltip", addTitleAttribute = TRUE,
            addCSS = NA, silent = missing(addCSS), ...)
}
\arguments{
  \item{file}{the name of the SVG file, the parsed XML document or an XML
      node or list of nodes}
  \item{text}{the text of the tooltip}
  \item{which}{the indices of the elements within \code{file} being
     given a tooltip. This is used when putting tooltips on points
     in a plot and is expected to be used to index both \code{text} and 
   \code{paths}.}
  \item{doc}{ the parsed XML document.}
  \item{save}{ a logical value or a file name that indicates whether the resulting
     document is to be written to a file. If this is TRUE, the name of the file is taken
     from the XML document (\code{doc}). If it is FALSE, the document is not 
     written to a file but returned for further processing and then serialized.
     If this is a character string, its (first) value is used as the name of the file to which to write the modified document.}
  \item{paths}{these are the nodes of the elements in the SVG document which are to be given the tooltips.}
  \item{elName}{either "title" or "desc" for the short or longer
    tooltip.}
  \item{addArea}{either a logical value or integer value of 0, 1, or 2.
    This indicates whether the function
    should attempt to make the entire area covered by each of the SVG
    elements active for the tooltip.

    If this is \code{NA}, the package functions try to infer what to do.
    
    If this is \code{FALSE}, only the actual pixels
    that are rendered, i.e. the path or stroke of the SVG element,
    will be active. If this is specified as \code{TRUE}, we try to
    make the entire area for the SVG element active by,
    e.g., adding an invisible enclosing rectangle "under" text
    or filling in a circle with the background color so
    as not to alter its appearance, but making it active.

    If this is \code{2}, we do the same operations as we do for
    \code{FALSE},  but also enclose the SVG element within an \code{<a>}
    node which has an attribute xlink:title with the tooltip as its
    value.  This then allows the tooltip to appear in Safari and Chrome,
    i.e. WebKit-based browsers.
  }
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
  \item{addTitleAttribute}{a logical value indicating whether to add a
  \code{xlink:title} attribute to the SVG element. The value of the
    attribute is the tooltip.  When this is \code{TRUE}, the tooltips
    for the resulting  SVG elements will appear in Firefox,
    i.e. Mozilla-based browsers.}
  \item{addCSS}{a logical value indicating whether to add the default
    CSS file for controlling the appearance of tooltip rectangles. If
    this is \code{NA}, we determine whether this is needed.  The code
    should only add each unique CSS file to the document once regardless of how many times the
    caller specifies that it should be added.}
  \item{silent}{a logical controlling whether \code{\link{addCSS}} emits
    a warning if there is a directive to load the  CSS file is already
    in the target document.
   }  
  \item{...}{additional parameters for methods}
}
\value{
  If \code{save} is FALSE, the updated XML document is returned.
  If \code{save} is TRUE or a string, the name of the file to which the document was written is
  returned.
}
\note{The interface to this function and its methods is likely to change and
become simpler.}
\author{Duncan Temple Lang}
\seealso{addAxesLinks}
\examples{

 doc = svgPlot(plot(mpg ~ wt, mtcars))
 addToolTips(getAxesLabelNodes(doc), c("Weight", "Miles per US Gallon"),
              addArea = TRUE)
 addToolTips(doc, rownames(mtcars),  addArea = TRUE,
              style = "fill: white; stroke: red;")


 doc = svgPlot(plot(mpg ~ wt, mtcars))
 tips = apply(mtcars, 1, 
               function(x) 
                 paste(names(mtcars), x, sep = " = ", collapse = ", "))
 addToolTips(doc, tips)
}
\keyword{iplot}
\keyword{aplot}
\keyword{dynamic}
\keyword{device}

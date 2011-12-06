\name{radioShowHide}
\alias{radioShowHide}
\title{Add toggle buttons to hide/display elements of an R SVG plot}
\description{
 This is a high-level, reasonably specialized function for adding interactive controls
 to an SVG document that dynamically (i.e. at viewing time) control the
 visiblity/invisibility of elements on the plot.
 It was developed to toggle the appearance of curves (time series) in a
 matplot and also across panels in a lattice plot.

 This uses the SVG widgets from Andreas Neumann to allow the viewer to
 toggle the elements displayed in the viewing region of the plot.
}
\usage{
radioShowHide(doc, insertScripts = TRUE, within = FALSE,
              group = FALSE, 
              labels = paste("Series", seq(length = numSeries)), 
              extraWidth = 15 * (max(nchar(labels)) + 1),
              save = if(!is(doc, "XMLInternalDocument")) doc, 
              id.prefix = "series",
              checkboxCallback = if (is.na(within))
                                   "togglePanel"
                                 else
                                    "toggle",
              jscripts = c("helper_functions.js",
                           "timer.js",
                           "checkbox_and_radiobutton.js",
                           "hideSVGElements.js"), 
              numPanels = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the SVG plot, given as either a file name or the XML tree
 returned from \code{\link{svgPlot}}.}
  \item{insertScripts}{a logical value indicating whether to insert the
    specified JavaScript code or merely use a reference to the files in
    the SVG document.}
  \item{within}{ a logical value that indicates whether the plot was created
    with a call to \code{\link[graphics]{matplot}} (\code{TRUE}) or a call to a
 regular plotting function (\code{FALSE}). This helps the function to
 identify the different elements in the plot.}
  \item{group}{unused!}
  \item{labels}{a character vector giving the labels to display beside
    the checkboxes. There should be one for each series/element in the plot. }
  \item{extraWidth}{the amount of horizontal space (in pixels?) that we add to the
     SVG viewbox to accomodate the check boxes.}
  \item{save}{a logical value that }
  \item{id.prefix}{a character string giving a value that is used as a
    prefix   for the \code{id} attribute in the different SVG elements
    that correspond to the series/elements whose visibility we want to toggle.}
  \item{checkboxCallback}{the name of the JavaScript function which is
      to be invoked at viewing time when the state of a check box is changed/toggled.}
  \item{jscripts}{a character vector giving the names of the JavaScript
   files needed to provide the viewing time functionality.}
  \item{numPanels}{the number of panels in the plot. This is relevant
   for lattice plots.}
}
\value{
  Either the updated SVG document or the name of the file
  to which it was written. The function is called for its
  side effect on the SVG document.
}
\references{\url{http://www.carto.net/papers/svg}}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{addSlider}} illustrates adding a different GUI component.  
}
\examples{

}
\keyword{iplot}
\keyword{aplot}

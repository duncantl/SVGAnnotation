\name{addSlider}
\alias{addSlider}
\title{Add slider to an SVG display}
\description{
  
  This function arranges to add an interactive slider to the SVG graphic
  described in \code{doc}.  The slider is implemented via the Carto.net
  SVG and ECMAScript/ECMAScript library.  This function endeavors to do
  most of the generic setup within the SVG document to be able to use
  the slider.

  The creator of the SVG document with the slider must ensure that there
  is enough space to display the slider (using
  \code{\link{enlargeSVGViewBox}}, for example). But often, this
  function provides the necessary functionality to get the slider into
  the document and SVG display and functional as most of the work is in
  the JavaScript.  }
\usage{
addSlider(doc, onload, javascript, id = "slider",
          svg = NULL,
          defaultJavascripts = c("mapApp.js",
                                 "helper_functions.js",
                                 "slider.js"), 
          side = "bottom", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the parsed XML/SVG document into which the relevant nodes will be inserted}
  \item{onload}{a JavaScript command which is invoked when the SVG document is loaded. This is expected
     to create the slider object.}
  \item{javascript}{any JavaScript code that is needed to support the \code{onload} computations or any other
    aspect of the view/run-time behavior of the SVG document. This is separate from
     the \code{defaultJavascripts} parameter as the files specified there are the standard/default ones
     that are needed to use the Carto.net slider generally. This \code{javascript} parameter is for the caller to specify
     code that is specific to the SVG document and the particular slider. }
  \item{id}{an identifying string/name for the slider. This should be unique within the SVG document.} % Is this used in SVG or JavaScript by Carto.net code.
  \item{svg}{the SVG root node of the XML document. This can be
    specified to allow a  caller who has already accessed this for other
    purposes to provide it so the function doesn't have to do the small
    amount of work to retrieve it.}
  \item{defaultJavascripts}{the names of the JavaScript code files that
    should also be included to make the slider function. These are
    passed to \code{\link{addECMAScripts}}.}
  \item{side}{where to place the slider}
  \item{\dots}{name = value pairs of JavaScript variable names and R
        objects that are also added to the JavaScript code. See \code{\link{addECMAScripts}}.}
}

\value{
 The updated XML document, returned invisibly.
}
\references{\url{http://www.carto.net/papers/svg}}
\author{Duncan Temple Lang}
\seealso{\code{\link{addECMAScripts}}}
\examples{

}
\keyword{programming}
\keyword{IO}
\concept{GUI}

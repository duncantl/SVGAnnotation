\name{addCSS}
\alias{addCSS}
\title{Use the Cascading Style Sheet within an SVG file}
\description{
 This function associates the Cascading Style Sheet with the specified
 (parsed) SVG document. It can either insert 
 a link or (h)reference to the CSS file (or URL)
 or can insert its contents directly into the SVG.
 Inserting the contents is useful if one wants to make the SVG independent of the location of the CSS file,
  e.g. if the CSS file is a local file and we want the SVG file to be distributed elsewhere.
 On the other hand, using a reference to the file allows us to have a single copy shared
 by multiple SVG files, and also facilitates updating the CSS file and having the changes
 be used by these SVG files without requiring additional processing.

 The CSS to be used is determined from the R option \code{"SVGCSS"}.
 If this is not set, we look at the environment variable \code{SVGCSS}.
 If that is not set, then we use the default which is in the
 installed \code{SVGAnnotation} package directories.
 
}
\usage{
addCSS(doc, css = getDefaultSVGCSS(),
       insert = inherits(css, "AsIs") || getOption("InsetScriptContents", FALSE), 
        silent = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the parsed SVG document}
  \item{css}{the name of the CSS file/URL. By default, we use the CSS file that comes with this
     package. The R user can specify a different one directly in the call to this function,
     or by setting the R option named SVGCSS, e.g. \code{options(SVGCSS = "mySVG.css")}.
  }
  \item{insert}{a logical value indicating whether to insert the contents of the
    CSS document into the XML document or add a reference to it.}
  \item{silent}{a logical value that controls whether a warning is
    emitted if one attempts to insert an already present CSS file.
    If this is \code{FALSE} (the default) a warning informs you;
    otherwise no message is issued.}  
}
\value{
  The updated XML document.
}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{addECMAScripts}}
  \code{\link{svgPlot}}
  
  \code{\link{compressStyles}} and
  \code{\link{convertCSSStylesToSVG}}
}
\examples{

  doc = svgPlot(plot(1:10))
  addCSS(doc)
  addCSS(doc)  # get a warning since it is already there.

     # add a reference
  addCSS(doc, "http://www.omegahat.org/OmegaTech.css", FALSE)

    # put the content in directly
  addCSS(doc, insert = TRUE)
     #
  addCSS(doc, I("class {\n  fill: red;\n  stroke: blue;}"))


  saveXML(doc, tempfile())
}
\keyword{iplot}
\keyword{aplot}
\keyword{dynamic}
\keyword{device}
\keyword{programming}

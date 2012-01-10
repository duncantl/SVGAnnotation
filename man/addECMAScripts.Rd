\name{addECMAScripts}
\alias{addECMAScripts}
\alias{addECMAScripts,XMLInternalDocument-method}
\alias{addECMAScripts,XMLInternalNode-method}
\alias{addECMAScripts,HTMLInternalDocument-method}
\alias{addECMAScripts,character-method}
\title{Add Java/ECMAScript code and data to an SVG document}
\description{

 This function and its methods allow the caller to easily and programmatically add
JavaScript/ECMAScript code to an SVG document or an HTML document. The functions take care
of the details, allowing the code to be inserted directly or a
reference to the file via an \code{href} link.  The code can also be
given as verbatim (AsIs) text rather than being in a file which allows
the content to be dynamically generated within R functions.

 The benefit of inserting the contents of a JavaScript file into
the document is that the resulting document does not have external
dependencies on this file. We can move the XML document and not have
to remember to also move the JavaScript file or update the link.

The downside is that it is often convenient to create the SVG and then
modify the JavaScript code.  This is very common when developing the
JavaScript code. If we keep the JavaScript file separate from the SVG,
we can update it and then merely reload the SVG file in our
viewer/browser to work with the updated JavaScript code.

An additional drawback to inserting the code is that if we use it
many different SVG files, we have many copies of the code and if
it is large, we are wasting space and bandwidth.

I tend to include the code after I have finished working out the bugs
and features, i.e. when I am generating the SVG file knowing it is
woring as desired.

 In addition to adding JavaScript code, one can also add JavaScript
variables which are created by serializing R objects. This allows us
to use data computed in R within our JavaScript code with little
effort. The serialization is done primarily via the \code{rjson}
package, with some generalizations provided by this package.
}
\usage{
addECMAScripts(doc, scripts,
               insertJS = inherits(scripts, "AsIs") || 
                             getOption("InsertScriptContents", FALSE),
               at = NA, ..., .jsvars = list(...),
               escapeFun = newXMLCDataNode)
}

\arguments{
  \item{doc}{the XML document to which the JavaScript code is to be added.
       This is expected to have been parsed previously rather than be the name of a file.}
  \item{scripts}{a character vector giving the names of the JavaScript code files
       or the code itself. Use \code{I("text for code")} to ensure the value is treated as verbatim code.}
  \item{insertJS}{a logical value which controls whether the JavaScript code is actually inserted into the 
     XML document so that it is a stand-alone document with no references to this JavaScript code,
     or if \code{href} references are inserted into the XML document.
     One can control whether the contents or the reference to the file is inserted via the 
     global option \code{InsertScriptContents}. Set this to \code{TRUE} to cause the contents to be inserted.
   }
  \item{at}{an integer that specifies where to add the new XML nodes within the children of the SVG root.
     By default, the new nodes are added at the end. The order is typically not important.}
  \item{\dots}{\code{name = value} pairs of JavaScript variable names and R objects which are to be serialized and assigned to those
    JavaScript variables.}
  \item{.jsvars}{an alternative to \code{\dots} in order to specify JavaScript variables to be defined and initialized.
    This is convenient when we have the variables already in a list.}
   \item{escapeFun}{either \code{NULL} or a function that is used to
     create the node to hold the text of the code. This is typically a
     CDATA node, but for HTML documents, it is a comment node}
}
\value{
  The updated XML document, invisibly.
}
\references{
 \url{http://www.w3.org/TR/SVG11/script.html}  
}
\author{Duncan Temple Lang}


\examples{
   # See linkPlots() for how to do this.
 data(mtcars)
 doc = svgPlot({
                 par(mfrow = c(1, 2))
                 plot(mpg ~ wt, mtcars)
                 plot(mpg ~ vs, mtcars)
               })

 f = system.file("JavaScript", "link.js", package = "SVGAnnotation")
 addECMAScripts(doc, f)
 addECMAScripts(doc, f, insert = TRUE)


 x = rnorm(20)
 y = rnorm(20)

 doc = svgPlot(plot(x, y))
 d = as.matrix(dist(cbind(x, y)))
 D = apply(d, 1, order)
 addECMAScripts(doc,
                "http://www.omegahat.org/SVGAnnotation/tests/knn.js",
                neighbors = D)
}
\keyword{IO}
\keyword{programming}


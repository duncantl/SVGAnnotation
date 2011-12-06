\name{annotateMosaic}
\alias{annotateMosaic}
\alias{getCategoryLabelNodes.mosaic}
\alias{getAxesLabelNodes.mosaic}
\title{Add tooltips to a Mosaic plot}
\description{
  The function \code{annotateMosaic} adds basic tooltips to a mosaic plot
  for visualizing frequency tables and conditional probabilities.
  The tooltips help viewers to comprehend what can be a complex
  plot.
  The tooltips currently give the count along with the different levels
  of the categorical variables.
  One can specify a function that is called to create the tooltip for
  each cell.

  Two lower level  functions are \code{getCategoryLabelNodes.mosaic}
  \code{getAxesLabelNodes.mosaic}.
  These return the SVG nodes corresponding to the
  labels for each category on the different sides of the plot
  and the SVG nodes for the axes/variable labels, respectively.

  These functions annotate the SVG nodes with meta-informatio about the
  type.   This allows us to retrieve the same nodes in subsequent calls
  even if we have modified the structure of the SVG document as we add
  markup such as tooltips and links.

  Note that for the current implementation, if any "row" in the
  mosaic plot has all cells with zero observations, the subsequent
  tooltips will be mis-aligned, i.e. some of the latter tooltips
  will be for the wrong (earlier) cells.
}
\usage{
annotateMosaic(doc, table, cellTips = NULL,
               axisLabelTips = c(names(attr(table, "col.vars")),
                                 names(attr(table, "row.vars"))),
               categoryTips = getMosaicLabels(table),
               shade = FALSE, addCSS = TRUE)
getCategoryLabelNodes.mosaic(doc, table = NULL,
                              numCells = prod(dim(table)), groupZeros = NA)
getAxesLabelNodes.mosaic(doc, table = NULL, groupZeros = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the SVG document, typically created via a call to
    \code{\link{svgPlot}}
    but this can also be the name of a file containing the SVG content
    or the already parsed document.}
  \item{table}{the frequency table returned by the call to
    \code{\link[vcd]{mosaic}}.
    This contains the information about the counts and the labels for
    the variables and their categories/levels and the order in which
    they were plotted.}
  \item{axisLabelTips}{the text for the axes labels tooltips}
  \item{categoryTips}{tooltips for the labels on the axes that identify
     the different categories of each of the categorical variables.}
  \item{shade}{a logical value indicating whether the \code{shade}
    parameter was specified as \code{TRUE} in the call to
    \code{\link[vcd]{mosaic}} which would then give rise to a legend. It
  is important to know this to interpret the nodes in the SVG content.}
%  \item{group}{}
  \item{addCSS}{a logical value indicating whether to add the (default)
    CSS to the SVG document or the name of a CSS file or CSS content
    to add.
  }
  \item{cellTips}{a character vector of tool tips for the cells in the
  plot or a function that is called to compute the tooltip string for a
  given cell. If this is a function it will be called with a single
  argument which is a data frame with one row giving the levels of the
  different categorical variables along with a variable named
  \code{.count} which gives the number of observations in that cell.}
  \item{numCells}{the number of cells in the mosaic plot. This can be
  specified instead of the \code{table}.}
  \item{groupZeros}{a logical value (or \code{NA}) indicating whether to
  perform the operation that groups SVG elements representing a cell
  with a zero count together into a single node. This makes processing
  the SVG easier.}
}
\value{
 The updated SVG document.
}
\references{
 The \code{vcd} package.
}
\author{
  Duncan Temple Lang
}

\note{At present, we can only handle mosaic plots with less than 5 variables.}

\seealso{
  \code{\link{svgPlot}}
}
\examples{
if(require(vcd)) {
  a = matrix(as.integer(rnorm(12, 100, 10)), 3, 4, dimnames = list(c("A", "B", "C"), c("W", "X", "Y", "Z")))
  names(dimnames(a)) = c("Var 1", "Var 2")
  saveXML(annotateMosaic(svgPlot(o <- mosaic(a)), o), "simpleMosaic.svg")



  saveXML(annotateMosaic(svgPlot( m <- mosaic(UCBAdmissions)), m), "ucb.svg")




  saveXML(annotateMosaic(svgPlot(z <- mosaic(Survived ~ ., data = Titanic)), z), "titanic.svg")




    # Example of 4 variables with more than 2 categories.
  xx = data.frame(a = sample(1:3, 100, TRUE), b = sample(c("A", "B", "C", "D"), 100, TRUE),
                  c = sample(c("X", "Y"), 100, TRUE), d = sample(c("Yes", "No"), 100, TRUE))

  saveXML(annotateMosaic(svgPlot(m <- mosaic(with(xx, table(a, b, c, d)))), m), "multiMosaic.svg")



    # Provide a function for creating the cell tooltip strings.
  saveXML(annotateMosaic(svgPlot(o <- mosaic(a)), o, cellTips = function(x) x$.count), "mySimpleMosaic.svg")



    # annotate the plot more manually, i.e. specifying links and
    # tooltips for the axes labels and category labels.

  data(UCBAdmissions)
  doc = svgPlot( m <- mosaic(UCBAdmissions))

  cats = getCategoryLabelNodes.mosaic(doc, m)
  ax <- getAxesLabelNodes.mosaic(doc, m)

#  annotateMosaic(doc, m, categoryTips = character())

  mapply(addLink, ax,
           c("http://www.math.yorku.ca/SCS/vcd/",
             "http://cran.r-project.org/web/packages/vcd/index.html",
             "http://www.omegahat.org/SVGAnnotation"))

  tips = c(Rejected = "The applicant was not admitted this year",
           Admitted = "The applicant was admitted ",
    F = "Department of ...",
    E = "Department of ...",
    D = "Department of ...",
    C = "Department of ...",
    B = "Department of ...",
    A = "Department of ...",
    Male = "Applicant was a man",
    Female = "Applicant was a woman")

  addToolTips(cats, tips[names(cats)])
  saveXML(doc, "iucb.svg")
} else {
  warning("You need the vcd package (Visualizing Categorical Data) for these mosaic examples")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
\keyword{graphics}


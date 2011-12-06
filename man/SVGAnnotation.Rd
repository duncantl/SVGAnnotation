\name{SVGAnnotation}
\alias{SVGAnnotation}
\title{The SVGAnnotation Package}

\description{
  This package provides facilities for annotating and enhancing
  static SVG documents created with R's regular graphics systems
  (grz, lattice and grid).
  We can add interactivity, animation and filtering effects.
  With SVG we can customize the appearance of the elements using
  Cascading Style Sheets, either as files or inlined into the SVG document.
  We can programmatically control the elements of the SVG
  at viewing time using ECMAScript/JavaScript code.
  We can embed the SVG within HTML (and other formats such as FO and
  hence PDF) and can access the objects with JavaScript
  in a bi-directional manner.

  This package has high-level facilities for annotating
  regular R plots such as scatterplots, pairs() plots,
  lattice plots, etc. These include
  \code{\link{animate}}, \code{\link{addToolTips}},
  \code{\link{addLink}}, \code{\link{addAxesLinks}}.

  The package also provides intermediate level tools for
  finding elements in the SVG document that correspond
  to components in the statistical display, e.g.
  axes labels, the title of a plot, the plot regions,
  the points within a plot region.
  Having found these elements, we can pass these elements to other high- to intermediate-level tools
  such as the ones we mentioned above to add the annotations.

  Functions such as \code{\link{addCSS}} and
  \code{\link{addECMAScripts}} allow us to insert content into the SVG
  to control the appearance and viewing time behavior, including
  serializing R objects as JavaScript variables that can be used at
  viewing time.

  There are additional, lower level facilities for working with aspects
  of the   SVG elements. For example, there are functions to
  extract, process the SVG path of an element and its sub-elements
  and to compute bounding boxes, the type of shape, etc.

  The help files for this package do provide reference material for
  the functions and some examples. However, the best place to find
  the high-level information about the functions and the
  package generally is to look at the relatively extensive
  set of examples. These are available online at
  \url{http://www.omegahat.org/SVGAnnotation/tests/examples.html}
}
\author{Duncan Temple Lang}
\keyword{programming}
\keyword{aplot}
\keyword{iplot}
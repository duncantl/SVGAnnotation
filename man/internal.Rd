\name{setIds}
\alias{setIds}
\title{Low-level functions for working with SVG plots from R}
\description{
 This is a collection of functions that others may want to use
 to post-process SVG plots created with R's cairo graphics device.

 \code{setIds} arranges to set an idattribute on each of the nodes.
  This can be used in subsequent processing of SVG content
  and most importantly in ECMAscript code that works with the
  SVG elements at view/run time.
}
\usage{
setIds(nodes, ids = seq(along = nodes), fix = c("", ""),
        sep = if (fix[1] == "") "" else "-")
}
\arguments{
  \item{nodes}{the SVG element nodes}
  \item{ids}{characters giving the values of the identifiers for the nodes. They should be unique.}
  \item{fix}{a prefix and suffix.}
  \item{sep}{separtor used between the prefix and suffix and the ids.}
}
\value{
  The identifiers strings.
}
\author{Duncan Temple Lang}
\keyword{iplot}
\keyword{aplot}
\keyword{dynamic}
\keyword{device}

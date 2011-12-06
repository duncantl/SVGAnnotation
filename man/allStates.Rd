\name{allStates}
\alias{allStates}
\docType{data}
\title{
County Election Results for the 2008 US Presidential Election
}
\description{
A data frame containing the county-level vote counts in the 2008 US presidential election 
}
\usage{data(electionHistory)}
\format{
This is a data frame with the county by county votes totals for the Repubican and 
Democratic candidates in the 2008 US presidential election. 

  \describe{
    \item{\code{Obama}}{a numeric vector of votes cast for Obama.}
    \item{\code{McCain}}{a numeric vector of votes cast for McCain.}
    \item{\code{county}}{a character vector indicating the name of the county.} 
    \item{\code{reporting}}{a numeric vector of the percent of recincts reporting.}
    \item{\code{state}}{a character vector indicating the name of the state.} 
  }
}
\note{
This is a reformatted version the data in the list called states which
is also found in election2008.rda. 
}
\note{
This is one of the objects found in election2008.rda. See the see also
section for related objects.
}
\source{
Scraped from...
}
\seealso{
        \code{\link{states}}
        \code{\link{exitData}}
}
\keyword{datasets}

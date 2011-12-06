\name{states}
\alias{states}
\docType{data}
\title{
County Election Results for the 2008 US Presidential Election
}
\description{
A list containing the county-level vote counts in the 2008 US presidential election.

Each element is a named data frame with the county by county votes
totals for the Repubican and Democratic candidates in the 2008 US
presidential election.  
} 

\usage{data(electionHistory)} 

\format{
  \describe{
    \item{\code{Obama}}{a numeric vector of votes cast for Obama.}
    \item{\code{McCain}}{a numeric vector of votes cast for McCain.}
    \item{\code{county}}{a character vector indicating the name of the county.} 
    \item{\code{reporting}}{a numeric vector of the percent of recincts reporting.}
  } 
}

\details{
This list contains the county-level vote totals for each state in
the 2008 US Presidential election.  

Each data frame contains the vote counts for the Republican and Democratic
candiates only. 
}
\note{
This is one of the objects found in election2008.rda. See the see also
section for related objects.
}
\source{
Scraped from...
}
\seealso{
        \code{\link{allStates}}
        \code{\link{exitData}}
}
\keyword{datasets}

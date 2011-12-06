\name{exitData}
\alias{exitData}
\docType{data}
\title{
Exit Poll Data for the 2008 US Presidential Election
}
\description{
A list containing summary statistics from state-level exit polls from
the 2008 US presidential election 
}

\usage{data(election2008)}
\format{
The data frames all have the same basic structure. They consist of 4 vectors.
  \describe{
    \item{\code{\% of Population}}{perenctage of state population in this category.}
    \item{\code{Obama}}{percent of respondents who said they voted for Obama.}
    \item{\code{McCain}}{percent of respondents who said they voted for McCain.}
    \item{\code{Group}}{levels of the group, e.g. for age the groups in Alabama exit poll} 
  }
}

\details{
This is a named list with one entry for each state.
Each element in the state list contains a list of summary statistics
from the exit poll in that state. 

The elements in the state list are data frames each of which corresponds to
the summary of the responses to a question in the exit poll.
Exit polls may have asked different questions.
}

\note{
The questions asked in the exit poll differ from state to state. 
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
        \code{\link{allStates}}
}

\keyword{datasets}

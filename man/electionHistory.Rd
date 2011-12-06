\name{electionHistory}
\alias{electionHistory}
\docType{data}
\title{
State by State Election Results Through History
}
\description{
A list containing the results of each presidential election between 1900 
and 2008.
}
\usage{data(electionHistory)}
\format{
This is a list. Each element is a named character vector 
with the state by state data on the party of the presidential
candidate who received the greatest popular vote. 
}
\details{
This list contains the results for each oh the 28 Presidential elections
between 1900 and 2008.

Each list element contains a named character vector in which the names
correspond to states (lower case) and the values correspond to the party
color of the winning candidate, i.e. "#FF0000" (red)
for Republican, "#0000FF" (blue) for Democrat, and "#00FF00" (green)
for other.
}
\note{
This is one of the objects found in ElectionHistory.rda. See the see also
section for related objects.
}
\note{
Not all list elements are the same length because the United States did not
contain all 50 current states until 1959.
}
\source{
Scraped from
http://uselectionatlas.org/RESULTS/
}
\keyword{datasets}

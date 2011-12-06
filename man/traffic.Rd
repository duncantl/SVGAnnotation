\name{traffic}
\alias{traffic}
\docType{data}
\title{Traffic Flow on the I-80E Sacramento at Loop Detector 313111
}
\description{
A data frame containing traffic flow information for the I-80E Sacramento
freeway during the 1 week period from midnight 1/04/2009 midnight to 
23:55 1/10/2009.
}
\usage{data(traffic)}
\format{
  \describe{
    \item{\code{Date}}{a character vector indicating date in the ##/##/#### format.}
    \item{\code{Timestamp}}{a character vector indicating time in the ##:##:## format.}
    \item{\code{Occ1}}{Occupancy in Lane 1}
    \item{\code{Flow1}}{Flow in Lane 1}
    \item{\code{Occ2}}{Occupancy in Lane 2}
    \item{\code{Flow2}}{Flow in Lane 2}
    \item{\code{Occ3}}{Occupancy in Lane 3}
    \item{\code{Flow3}}{Flow in Lane 3}
    \item{\code{Points}}{Number of detectors used to generate this observation}
    \item{\code{PercentObserved}}{Percent of observations successfully gathered during the 5 minute period.}
  }
}
\details{
All measurements were taken during 5 minute measuring periods.

Occupancy in this context is defined as the proportion of time during the 5 minute
period that a vehicle was present on the loop detector.

Flow is defined as the number of distinct vehicles which passed over the 
loop detector during the 5 minute measuring period.
}
\source{
Performance Measurement System
http://pems.dot.ca.gov
}
\keyword{datasets}

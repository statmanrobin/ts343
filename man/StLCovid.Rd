\name{StLCovid}
\alias{StLCovid}
\docType{data}
\title{
St. Lawrence County Covid-19 Cases
}
\description{
Cumulative and daily positive covid-19 cases and deaths, starting 3/22/2020
}
\usage{data("StLCovid")}
\format{
  A data frame with 281 observations on the following 5 variables.
  \describe{
    \item{\code{Date}}{Date in mm/dd format}
    \item{\code{CumPositive}}{Cumulative positive covid-19 cases}
    \item{\code{DayPositive}}{New positive covid-19 cases reported that day}
    \item{\code{CumDeath}}{Cumulative covid-19 deaths}
    \item{\code{DayDeath}}{New covid-19 deaths reported that day}
  }
}
\details{
Counts of positive cases (dialy and cumualative) and deaths (cumulative and daily) due to covid-19 in St. Lawrence county.
}
\source{
Data extracted from county level covid-19 data maintaine by the NY Times at https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv.
}

\keyword{datasets}

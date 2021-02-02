\name{USCovid}
\alias{USCovid}
\docType{data}
\title{
US Covid-19 (Daily)
}
\description{
Daily cases and deaths in the US due to Covid-19 (2/1/2020 to 1/31/2021)
}
\usage{data("USCovid")}
\format{
  A data frame with 366 observations on the following 6 variables.
  \describe{
    \item{\code{Date}}{Daye in yyyy-mm-dd format}
    \item{\code{CumCases}}{Cumulative positive cases}
    \item{\code{CumDeaths}}{Cumulative deaths}
    \item{\code{t}}{Time index (1 to 366)}
    \item{\code{Cases}}{New daily postive cases}
    \item{\code{Deaths}}{New dauly deaths}
  }
}
\details{
Daily new and cumulative cases and deaths due to Covid-19 in the United States for one year (2/1/2020 to 1/31/2021).
}
\source{
Data downloaded from the NY Times github site https://github.com/nytimes/covid-19-data
}

\keyword{datasets}

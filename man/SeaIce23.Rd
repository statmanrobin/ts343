\name{SeaIce23}
\alias{SeaIce23}
\docType{data}
\title{
Arctic Sea Ice (1979-2023)
}
\description{
Yearly area and extent of Arctic sea ice measured in September(1979-2023)
}
\usage{data("SeaIce23")}
\format{
  A data frame with 45 observations on the following 4 variables.
  \describe{
    \item{\code{Year}}{Year (1979 to 2023)}
    \item{\code{Extent}}{Extent of the Arctic sea ice (in millions of square km)}
    \item{\code{Area}}{Area of the Arctic sea ice (in millions of square km)}
    \item{\code{t}}{Time index (1 to 45)}
  }
}
\details{
Extent and area of Arctic sea ice measured in September each year. Extent is measured from the outside perimeter, while area deducts internal regions of open water.  Estimates are made from satellite images.
}
\source{
Data from https://noaadata.apps.nsidc.org/NOAA/G02135/seaice_analysis/ updated data from

Witt, G. (2103) "Using Data from Climate Science to Teach Introductory Statistics," JSE 21:1 available at www.amstat.org/publications/jse/v21n1/witt.pdf
}

\keyword{datasets}

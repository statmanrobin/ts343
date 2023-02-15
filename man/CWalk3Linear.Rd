\name{CWalk3Linear}
\alias{CWalk3Linear}
\docType{data}
\title{
Simulated Stock Prices from Four Models
}
\description{
Data simulated from Constant, Random Walk, Model #3, and Linear models
}
\usage{data("CWalk3Linear")}
\format{
  A data frame with 25 observations on the following 6 variables.
  \describe{
    \item{\code{at}}{Random errors using N(0,0.25)}
    \item{\code{Constant}}{Constant model with mean=20}
    \item{\code{RWalk}}{Random walk starting at 20}
    \item{\code{Model3}}{Model #3 with theta=0.5 and mean=20}
    \item{\code{Linear}}{Linear model with intercept=20, slope=0.1}
    \item{\code{t}}{Time indicator, starting at t=1}
  }
}
\details{
We simulated stock prices with four different models.  All use the same set of randomly generated erorrs from a N(0,0,25) distribution.
}
\source{
Generated from class simulation
}

\keyword{datasets}

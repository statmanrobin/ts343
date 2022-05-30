\name{psij}
\alias{psij}

\title{
Finding the psi_j weights of ARIMA Models
}
\description{
Takes an ARIMA model and produces a vector of psi_j weights for the fitted model
}
\usage{
psij(model, inf = "ma", deg = 6, digits = 4)
}

\arguments{
  \item{model}{
an object of class “arima”. For example, model produced by the Arima() function}
  \item{inf}{The type of infinite expansion in which the user wants to view the fitted ARIMA model. The default is “ma” which returns a vector of the psi_j weights for a MA infinity model. Other options for form include “ar”}
  \item{deg}{how many psi_j weights the user selects. Range of 1-8 }
  \item{digits}{The number of digits the user rounds each psi_j weight}
}
\details{
Calculates the psi_j weights by taking the AR and MA characteristic polyonomials, uses the taylor() function to find the Taylor Series approximation of the quotient of the two polynomials and returns a vector of the first 6 coefficients.
}
\value{
A vector of psi_j weights for a given fitted ARIMA model
}

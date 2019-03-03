#' Simulate a Random Walk
#'
#' @param n  Length of the series to simulate
#' @param z0  Starting point for the series
#' @param drift Value for a drift parameter (default is zero)
#' @param sderror Standard deviation of the error term
#'
#' @return A time series simulated from the model
#'
#' @export
#'

simWalk=function(n=100,z0=0,drift=0,sderror=1){
  at=rnorm(n,0,sderror)
  round(ts(z0+cumsum(at)),2)
}

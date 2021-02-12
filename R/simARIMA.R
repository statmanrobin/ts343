#' Simulate data from an ARIMA model
#'
#' @param n  Length of the series to simulate
#' @param delta  Constant term (delta)
#' @param phi AR coefficients (use c(  ) to enter more than one)
#' @param theta Moving average coefficients (use c( ) to enter more than one)
#' @param d Number of differences (default is d=0)
#' @param sderror Standard deviation of the error term
#'
#' @return A time series simulated from the ARIMA(p,d,q) model
#'
#' @export
#'

simARIMA=function(n=100,delta=0,phi=NULL,d=0,theta=NULL,sderror=1){
  p=length(phi)
  q=length(theta)
  if(!is.null(theta)){theta=-theta}
  z=arima.sim(n=n,model=list(order=c(p,d,q),ar=phi,ma=theta),sd=sderror)
  if(!is.null(phi) && phi!=0){mean=delta/(1-sum(phi))} else {mean=0}
  z+mean
}

#' Simulate a First order Autoregressive Model AR(1)
#'
#' @param n  Length of the series to simulate
#' @param delta  Constant term (delta)
#' @param phi Coefficient of Z(t-1)
#' @param sderror Standard deviation of the error term
#'
#' @return A time series simulated from the model
#'
#' @export
#'

simAR1=function(n=100,delta=0,phi=0.5,sderror=1){
  mean=delta/(1-phi)
  arima.sim(model=list(ar=phi),n=n,sd=sderror)+mean
}

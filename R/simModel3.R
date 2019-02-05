#' Simulate an MA(1) model (Model #3)
#'
#' @param n  Length of the series to simulate
#' @param mean  Mean (theoretical) of the series
#' @param theta Coeffiecient of error a(t-1)
#' @param sderror Standard deviation of the error term
#'
#' @return A time series simulated from the model
#'

simModel3=function(n=100,mean=0,theta=0.5,sderror=1){
  arima.sim(model=list(ma=theta),n=n,sd=sderror)+mean
}

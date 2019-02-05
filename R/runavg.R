#' Create a running average of past values for a historical time series.
#'
#' @param z  A time series or vector of values
#' @param k  Number of previous terms to average
#' @param na.rm Remove the inital NA values (default is TRUE)
#'
#' @return A time series (or vector) of the averages taken k at a time with the first value at time k+1
#'
#' @export
#'

runavg=function(z,k,na.rm=TRUE){
  a=stats::filter(z,rep(1/(k+1),k+1),sides=1)
  b=(1/k)*((k+1)*a-z)
  if(na.rm) {b=na.omit(b)}
  return(b)
}


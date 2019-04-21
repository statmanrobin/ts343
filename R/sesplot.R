#' Plot forecasts (historical and future) for exponential smoothing models
#'
#' @param mod  The result of ftting an exponential smoothing moel with ses( ), holt( ) or hw( )
#' @param include A number of recnt past values to show (default is NULL for all)
#' @param attach Attach the future forecasts (and bounds) the the historical "fits" (default), or historical "series", or "no" attaching
#' @param main Usual paramter to enter a title for the graph
#' @param lwd Control line width (default is lwd=2)
#'
#' @return A plot with the series, historical (smoothed) values, forecast (and bounds) for future values. 
#'
#' @export
#'
#'
sesplot=function(mod,include=NULL,attach="fits",main="",lwd=2){
  x=mod$x
  n=length(x)
  past=mod$fitted
  upper=mod$upper
  lower=mod$lower
  nextfit=mod$mean
  #adjust to show only the last "include" values of the historical series
  if (!is.null(include)) {
    x=subset(x,start=n-include+1)
    past=subset(past,start=n-include+1)
  }
  #Below to attach future forecasts to the historical series
  if (attach=="series"){
    nextfit=ts(c(tail(x,1),nextfit),start=end(x),freq=frequency(x))
    lower=ts(c(tail(x,1),lower),start=end(x),freq=frequency(x))
    upper=ts(c(tail(x,1),upper),start=end(x),freq=frequency(x))
  }
  #Below to attach future forecasts to the forecasts for historical series (default)
  if(attach=="fits"){
    nextfit=ts(c(tail(past,1),nextfit),start=end(past),freq=frequency(past))
    lower=ts(c(tail(past,1),lower),start=end(past),freq=frequency(past))
    upper=ts(c(tail(past,1),upper),start=end(past),freq=frequency(past))
  }
  ts.plot(x,past,nextfit,lower,upper,
          col=c("black","blue","blue","red","red"),lwd=lwd,main=main)
}

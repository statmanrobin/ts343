#'Compute the autocorrelation of a time series
#'
#' @param series A time series
#' @param maxlag Number of autocorrelations to return (Null default picks a reasoanble number)
#' @param plot Plot of the ACF with bounds? (default is FALSE)
#' @param ndiff Number of regualr differences (default is 0)
#' @param sdiff Number of seasonal differences (default is zero)
#' @param out Output the numerical ACF? (default is TRUE)
#' @param ci.type Type of confidence bounds ("ma" defualt or "white" for white noise)
#'
#' @return An autocorrelation object with the first component(lag 0) set to NA
#'
#' @export
#'

sluacf=function(series,maxlag=NULL,plot=FALSE,ndiff=0,sdiff=0,out=TRUE,ci.type="ma"){
  sname=deparse(substitute(series))
  if(ndiff>0){series=diff(series,differences=ndiff)
  sname=paste(sname,"d=",ndiff)}
  if(sdiff>0){series=diff(series,differenes=sdiff,lag=frequency(series))
  sname=paste(sname,"D=",sdiff)}
  seriesacf=acf(series,lag.max=maxlag,plot=F)
  seriesacf$series=sname
  seriesacf$acf[1]=NA
  if(frequency(series)>1){seriesacf$lag=seriesacf$lag*frequency(series)}
  if(plot){
    marold=par("mar")    #adjust margins to handle title better
    par(mar=c(5.1,4.1,4.1,2.1))
    plot(seriesacf,ci.type=ci.type,xlim=c(1,length(seriesacf$acf)-1))
    par(mar=marold)}
  if(out){return(seriesacf)}
}

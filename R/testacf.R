#' Provide test statistics for autocorrelations (individual and LBQ)
#'
#' @param seriesacf  The result of an autocorreltion calculation, e.g. from acf( ) or sluacf( )
#'
#' @return A dataframe with lag, autocorrelation (rk), std. error of rk (Srk), test statistic (tsrk) and Ljung-Box statistic (LBQ)
#'

testacf=function(seriesacf){
  rk=seriesacf$acf[-1]             #Get rid of NA at the start
  n=seriesacf$n.used               #find the sample size for the series
  rksq=rk^2
  Srk=sqrt((1+2*cumsum(rksq))/n)
  Srk=c(sqrt(1/n),head(Srk,-1))     #adjust to account for sum to k-1
  tsrk=rk/Srk
  lag=1:length(rk)
  LBQ=n*(n+2)*cumsum(rksq/(n-lag))
  rk=round(rk,3)
  Srk=round(Srk,5)
  tsrk=round(tsrk,2)
  LBQ=round(LBQ,2)
  data.frame(lag,rk,Srk,tsrk,LBQ)
}

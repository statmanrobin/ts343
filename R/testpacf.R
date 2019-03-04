#' Provide test statistics for partial autocorrelations
#'
#' @param seriespacf  The result of a partial autocorreltion calculation, e.g. from pacf( ) or slupacf( )
#'
#' @return A dataframe with lag, partial autocorrelation (rkk), std. error of rkk (Srkk), test statistic (tsrkk)
#'
#' @export
#'

testpacf=function(seriespacf){
  rkk=seriespacf$acf
  n=seriespacf$n.used               #find the sample size for the series
  Srkk=1/sqrt(n)
  lag=1:length(rkk)
  tsrkk=rkk/Srkk
  rkk=round(rkk,3)
  Srkk=round(Srkk,5)
  tsrkk=round(tsrkk,2)
  data.frame(lag,rkk,Srkk,tsrkk)
}

#' Provide test statistics for partial autocorrelations
#'
#' @param seriesacf  The result of a partial autocorreltion calculation, e.g. from pacf( ) or slupacf( )
#'
#' @return A dataframe with lag, partial autocorrelation (phikk), std. error of phikk (Sphikk), test statistic (tsphikk) and Ljung-Box statistic (LBQ)
#'
#' @export
#'

testpacf=function(seriespacf){
  phikk=seriespacf$acf
  n=seriespacf$n.used               #find the sample size for the series
  Sphikk=1/sqrt(n)
  lag=1:length(phikk)
  tsphikk=phikk/Sphikk
  phikk=round(phikk,3)
  Sphikk=round(Sphikk,5)
  tsphikk=round(tsphikk,2)
  data.frame(lag,phikk,Sphikk,tsphikk)
}

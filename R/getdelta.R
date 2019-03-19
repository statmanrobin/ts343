#' Compute the estimate for the constant term (delta) when fitting  ARIMA(p,0,q)
#'
#' @param armamodel  The result of an ARIMA(p,0,q) fit with constant term included
#'
#' @return The estimated value for delta, he constnat term in the ARIMA(p,0,q) model
#' @export
#'


getdelta=function(armamodel){
  terms=armamodel$arma
  p=terms[1]
  P=terms[3]
  q=terms[2]
  Q=terms[4]
  coeff=armamodel$coef
  mu=coeff["intercept"]
  if (is.na(mu)) {delta=0}
    else{
      sumar=0
       if (p>0) {sumar=sumar+sum(coeff[1:p])}
       if (P>0) {sumar=sumar+sum(coeff[p+q+1:p+q+P])}
      delta=mu*(1-sumar)
    }
  return(as.numeric(delta))

}
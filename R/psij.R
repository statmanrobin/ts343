library(pracma)


#' Finding the psi_j weights of ARIMA Models
#'
#' @param model an object of class “arima”. For example, model produced by the Arima() function
#' @param inf  The type of infinite expansion in which the user wants to view the fitted ARIMA model. The default is “ma” which returns a vector of the psi_j weights for a MA infinity model. Other options for form include “ar”
#' @param deg how many psi_j weights the user selects. Range of 1-8
#' @param digits The number of digits the user rounds each psi_j weight
#'
#' @return A vector of psi_j weights for a given fitted ARIMA model
#' @export
#'
psij = function(model, inf = "ma", deg = 6, digits = 4){

  fit = polyChar(model)

  #Multiplying each characteristic polynomials to make 1 Z_t and 1 a_t polynomials
  c_zt = fit[[1]]*fit[[2]]*fit[[3]]*fit[[4]]
  c_at = fit[[5]]*fit[[6]]

  #Making the Z_t and a_t polynomials a function
  p1= c_at
  p2= c_zt
  f1=as.function(p1)
  f2=as.function(p2)


  if(inf == "ma"){

    f=function(x) {f1(x)/f2(x)}
    out = rev(round(taylor(f, 0, n = deg),digits))
    out = out[-1]
    return(out)

  }

  if(inf == "ar"){

    f=function(x) {f2(x)/f1(x)}
    out = rev(round(taylor(f, 0, n = deg),digits))
    out = out[-1]
    return(out)

  }


}

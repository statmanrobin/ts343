

library(PolynomF)

#' Show characteristic polynomials from an Arima model
#'
#' @param model an object of class “arima”. For example, model produced by the Arima() function
#'
#' @return A list of characteristic polynomials for a given fitted ARIMA model
#' @export
#'
#'
polyChar = function(model){
  
  
  #The following code extracts the coefficients from the Arima output
  outcoefs = round(model$coef, 3)
  outNames = names(model$coef)
  x <- c("^ar", "^ma","^sar","^sma")
  
  #The coefficients are stored as vectors
  vec = sapply(x, function(y) grep(y, outNames))
  ar = c(1, -1*outcoefs[vec[["^ar"]]])
  ma = c(1, outcoefs[vec[["^ma"]]])
  sar = c(1, -1*outcoefs[vec[["^sar"]]])
  sma = c(1, outcoefs[vec[["^sma"]]])
  
  #Storing values of ARIMA(p,d,q)X(P,D,Q)
  p <- model$arma[1]
  q <- model$arma[2]
  P <- model$arma[3]
  Q <- model$arma[4]
  S <- model$arma[5]
  d <- model$arma[6]
  D <- model$arma[7]
  
  # Vectors of coeffiecients are then store in a list
  finalList = list(ar,ma,d,sar,sma)
  
  polyList = list()
  
  #Using the list of vectors, polynomials are created
  
  # ar polynomial
  p_ar = polynom(a = finalList[[1]])
  polyList[[1]] <- p_ar
  
  
  # sar polynomial
  
  vsar <- c(1)
  
  if (P != 0){
    for (i in 1:P){
      vsar = c(vsar, rep(0, S-1), finalList[[4]][i+1])
    }
  }
  
  p_sar = polynom(a = vsar)
  polyList[[2]] <- p_sar
  
  # d polynomial
  p_d <- polynom(a = c(1,-1))^d
  polyList[[3]] <- p_d
  
  # D polynomial
  
  p_D <- polynom(a = c(1,rep(0,S-1),-1))^D
  polyList[[4]] <- p_D
  
  
  # ma polynomial
  p_ma = polynom(a = finalList[[2]])
  polyList[[5]] <-  p_ma
  
  
  # sma polynomial
  
  vsma <- c(1)
  
  if (Q != 0){
    for (i in 1:Q){
      vsma = c(vsma, rep(0, S-1), finalList[[5]][i+1])
    }
  }
  
  p_sma = polynom(a = vsma)
  polyList[[6]] <- p_sma
  
  # constant term
  
  delt = c(getdelta(model))
  fdelt = polynom(a = delt)
  polyList[[7]] <- fdelt
  
  names(polyList) = c("ar", "seasonal ar","diff","seasonal diff","ma", "seasonal ma", "constant")
  
  return(polyList) #returns a list of polynomials
  
}
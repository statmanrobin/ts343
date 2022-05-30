
library(PolynomF)    #Gives an error if PolynomF is not installed?

#' Show characteristic polynomials from an Arima model
#' 
#' @description The polyChar function takes an ARIMA model and produces a list of characteristic polynomials for the fitted model. The elements in the list are of class “polynomial”. The terms are auto-regressive (ar), seasonal auto-regressive (sar), regular differences (diff), seasonal differences (s diff), moving average (ma), seasonal moving average (sma), and the constant term.
#' 
#' @param model  an object of class “arima”. For example, model produced by the Arima() function
#' 
#' @details polyChar extracts the coefficients from the ARIMA output and creates a vector of 1 and the coefficient(s) of each term (Example: ma = c(1, coef, coef2)). It then places each vector into a list (finaList). Then using the polynom function from the PolynomF package, polyChar then takes each vector from the list and uses each element within the vector to create a polynomial with ascending degree by the index. Then, an empty polynomial list is created (polyList). The polynomials are assigned an index, named according to their corresponding term in the ARIMA model, and added to polyList. If the term is not present in the model, it defaults to a 1 in the polynomial list. The function then returns the polyList. The list will return a list of 7 elements of the class polynomial.
#' 
#' @return A list of characteristic polynomials for a given fitted ARIMA model
#' 
#' @export

polyChar = function(model){
  
# Code below can check for PolynomF pacakge
#  if (!requireNamespace("PolynomF", quietly = TRUE)) {
#    stop("Package \"PolynomF\" needed for this function to work. Please install it.",
#         call. = FALSE)
#  } 
  
  #The following code extracts the coefficients from the Arima output
  outcoefs = round(model$coef, 3)
  outNames = names(model$coef)
  x <- c("^ar", "^ma","^sar","^sma")
  
  #The coefficients are stored as vectors
  vec = sapply(x, function(y) grep(y, outNames))
  ar = c(1, outcoefs[vec[["^ar"]]])
  ma = c(1, outcoefs[vec[["^ma"]]])
  sar = c(1, outcoefs[vec[["^sar"]]])
  sma = c(1, outcoefs[vec[["^sma"]]])
  
  #Storing values of ARIMA(p,d,q)X(P,D,Q)
  p <- model$arma[1]
  q <- model$arma[2]
  P <- model$arma[3]
  Q <- model$arma[4]
  S <- model$arma[5]
  d <- model$arma[6]
  D <- model$arma[7]
  
  # Vectors of coefficients are then store in a list
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
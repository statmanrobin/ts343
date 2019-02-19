#' Print a more standard ANOVA table for a regression model
#'
#' @param model  Result from an lm( ) regression mdoel
#'
#'
#' @export
#'

slunova=function(model){
  numpred=model$rank-1
  dferror=df.residual(model)
  dfmodel=numpred
  dftotal=dfmodel+dferror
  sse=sum(model$residual^2)
  ssmodel=sum(model$effects[2:(numpred+1)]^2)
  sstotal=ssmodel+sse
  msmodel=ssmodel/dfmodel
  mse=sse/dferror
  fstat=msmodel/mse
  pvalue=1-pf(fstat,dfmodel,dferror)
  df=c(dfmodel,dferror,dftotal)
  ss=c(ssmodel,sse,sstotal)
  ms=c(msmodel,mse,0)
  f=c(fstat,0,0)
  p=c(pvalue,0,0)
  table=data.frame(df,ss,ms,f,p)
  table[2,4:5]=NA
  table[3,3:5]=NA

colnames(table)=c("Df","Sum Sq","Mean Sq","F value","P(>F)")
  row.names(table)=c("Model","Error","Total")
  class(table)=c("anova","data.frame")
  structure(table,
            heading=c("ANOVA Table",                            paste("Model:", formula(model)[2],formula(model)[1],formula(model)[3],"\n")))
}

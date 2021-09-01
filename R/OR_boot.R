#' Function to apply the bootstrap method to calculate the standard error of the odds ratio (S-sign RI method)

#' @param formula is the model to be fitted (PO~group + auxiliary)
#' @param data is a dataframe with outcome variable PO in first column, group in second, and auxiliary variables

#' @return estimate of the Marginal log Odds ratio
#' @import logistf
#' @export

OR_boot<- function(formula, data, indices) {

  data <- data[indices,]
  m  <- logistf(formula, data = data,control= logistf.control( maxit=5000))

  n<-dim(data)[1]

  newdata = data.frame(data[,-1])
  newdata0 = data.frame(data[,-1])
  newdata0$group = rep(0,n)
  newdata1 = data.frame(data[,-1])
  newdata1$group = rep(1,n)

  pred<-predict(m,newdata=newdata,type="response")
  pred0<-predict(m,newdata=newdata0,type="response")
  pred1<-predict(m,newdata=newdata1,type="response")

  Q1 <- mean(pred1)

  Q0 <- mean(pred0)


  MOR <- log((Q1 * (1 - Q0)) / ((1 - Q1) * Q0) )

  MOR
}

#' Probabilistic index model (PIM) for the conditional R-null hypotheses


#' @param formula is the conditional PIM to fit (Ratio ~ group + auxiliary variables)
#' @param data is a dataframe with outcome variable RATIO in first column, grouping variable in the second column and auxiliary variables in the following columns

#' @return list object with 1) fitted PIM 2) estimated PIM coefficients 3) estimated variance-covariance matrix 4) Teststatistic 5) p-value
#' @import pim
#' @export


R_sign_Conditional<-function(formula,data) {

  #RATIO[is.infinite(RATIO)]<-9999999
  #RATIO[is.na(RATIO)]<-9999999

  est<-pim(formula,data=na.omit(data),link="logit",estim =estimator.glm,vcov.estim = sandwich.vcov)

  est_coef <- coef(est)
  est_var <- vcov(est)

  est_teststat <- est_coef[1] / sqrt(est_var[1,1])
  est_pval <- 2*pnorm(-abs(est_teststat))

  res <- list(Fit=est,Coefficients= est_coef, Variance = est_var, Teststatistic = est_teststat, Pval=est_pval)

  return(res)
}

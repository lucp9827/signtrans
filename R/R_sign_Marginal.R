#' Probabilistic index model (PIM) for the marginal R-null hypotheses

#' @param data is a dataframe with outcome variable RATIO in first column and grouping variable in the second column

#' @return list object with 1) fitted PIM 2) estimated PIM coefficients 3) estimated variance-covariance matrix 4) Teststatistic 5) p-value
#' @import pim
#' @export


R_sign_Marginal<-function(data){
  # RATIO<-as.vector(RATIO)
  #
  # RATIO[is.infinite(RATIO)]<-9999999
  # RATIO[is.na(RATIO)]<-9999999

  est<-pim(formula=RATIO~ as.factor(group)  ,link="logit",estim =estimator.glm,vcov.estim = sandwich.vcov,keep.data = TRUE,data=data)

  est_coef <- coef(est)
  est_var <- vcov(est)

  est_teststat <- est_coef[1] / sqrt(est_var[1,1])
  est_pval <- 2*pnorm(-abs(est_teststat))

  res <- list(Fit=est,Coefficients= est_coef, Variance = est_var, Teststatistic = est_teststat, Pval=est_pval)

  return(res)
}

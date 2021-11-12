#' Regression imputation method for the marginal S-null hypotheses

#' @param formula is the conditional logistic regression model to fit (PO ~ group + auxiliary variables)
#' @param data is a dataframe with outcome variable PO in first column, grouping variable in the second column and auxiliary variables in the following columns

#' @return list object with 1) estimated logistic regression coefficients 2) estimated variance-covariance matrix 3) Teststatistic 4) p-value
#' @import boot
#' @export

S_sign_RI<-function(formula,data) {

  OR_boot_values <- boot(data = data, statistic = OR_boot,R = 100,formula = formula) # OR_boot needs adjustment depending on auxiliary variables

  est_coef <- mean(OR_boot_values$t)
  est_var <- var(OR_boot_values$t)

  est_teststat <- est_coef / sqrt(est_var)
  est_pval <- 2*pnorm(-abs(est_teststat))

  res <- list(Coefficients= est_coef, Variance = est_var, Teststatistic = est_teststat, Pval=est_pval)

  return(res)
}

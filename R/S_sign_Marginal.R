#' Logistic regression model for the marginal S-null hypotheses

#' @param data is a dataframe with outcome variable PO in first column and grouping variable in the second column

#' @return list object with 1) fitted logistic regression model 2) estimated logistic regression coefficients 3) estimated variance-covariance matrix 4) Teststatistic 5) p-value
#' @import logistf
#' @export

S_sign_Marginal <- function(data) {

est <- logistf(PO ~ as.factor(group) , control= logistf.control( maxit=5000),data=data)
est_coef <- est$coefficients
est_var <- vcov(est)

est_teststat <- est_coef[2] / sqrt(est_var[2,2])
est_pval <- 2*pnorm(-abs(est_teststat))

res <- list(Fit=est,Coefficients= est_coef, Variance = est_var, Teststatistic = est_teststat, Pval=est_pval)

return(res)
}

#' Regression imputation method for the marginal S-null hypotheses

#' @param formula is the conditional logistic regression model to fit (PO ~ group + auxiliary variables)
#' @param data is a dataframe with outcome variable PO in first column, grouping variable in the second column and auxiliary variables in the following columns

#' @return list object with 1) estimated logistic regression coefficients 2) estimated variance-covariance matrix 3) Teststatistic 4) p-value
#' @import boot
#' @export

S_sign_RI<-function(formula,data,procedure="boot",B=100) {

  if (procedure =="boot"){
  OR_boot_values <- boot(data = data, statistic = OR_boot,R = B,formula = formula) # OR_boot needs adjustment depending on auxiliary variables

  est_coef <- mean(OR_boot_values$t)
  est_var <- var(OR_boot_values$t)

  est_teststat <- est_coef / sqrt(est_var)
  est_pval <- 2*pnorm(-abs(est_teststat))
  }
  if (procedure =="ML"){
    m  <- logistf(formula, data = data,control= logistf.control( maxit=5000), plcontrol= logistpl.control( maxit=10000),pl=FALSE)

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

    #MOR <- (Q1 * (1 - Q0)) / ((1 - Q1) * Q0)
    MOR <- log((Q1 * (1 - Q0)) / ((1 - Q1) * Q0))

    est_coef = MOR

    # Variance estimation using efficient influence curve ! o

    gW<-sum(data[,'group']==1)/length(data[,'group'])

    D1 <- (data[,'group']/gW) *(data[,'PO']- pred1) + pred1 - Q1

    D0 <- ((1-data[,'group'])/(1-gW )) * (data[,'PO']- pred0) + pred0 - Q0

    EIC <- (1/(Q1*(1-Q1))) * D1 - (1/(Q0*(1-Q0))) * D0

    est_var<-sum(EIC^2)/n^2

    est_teststat<-(est_coef)/(sqrt(est_var))

    est_pval<-2*pnorm(-abs(est_teststat))
    }

  res <- list(Coefficients= est_coef, Variance = est_var, Teststatistic = est_teststat, Pval=est_pval)

  return(res)
}

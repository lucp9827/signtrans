#' Regression imputation method for the marginal R-null hypotheses

#' @param formula is the conditional probabilistic index model to fit (RATIO ~ group + auxiliary variables)
#' @param data is a dataframe with outcome variable RATIO in first column, grouping variable in the second column and auxiliary variables in the following columns

#' @return list object with 1) estimated PIM coefficient 2) estimated variance 3) Teststatistic 4) p-value
#' @import pim
#' @export

R_sign_RI<-function(formula, data,alpha=0.05){

   RATIO = data$RATIO
   group = data$group
   x = data[,-c(1,2)]

  n<-length(data$RATIO)
  p<-sum(data$group==1)/n

  x<-as.matrix(x)
  # Point estimate:
  pim.fit<-pim(formula,link="logit",data=data,estim =estimator.glm,vcov.estim = sandwich.vcov)#.sub
  coef<-coef(pim.fit)
  #summary(pim.fit)
  augMW<-0
  for(i in 1:n){

    fac = expit(coef[1]+t(-x[i,]+t(x[-i,]))%*%coef[2:(dim(x)[2]+1)])
    #fac[fac=='NaN',]= 1
    augMW<-augMW+sum(fac)/(n*(n-1))}

  # Standard error:
  pseudo.y<-pseudo(RATIO)
  a1.hat<-sapply(A=group,pseudo.Y=pseudo.y,1:length(group),vec.a1hat)
  a2.hat<-sapply(A=group,pseudo.Y=pseudo.y,1:length(group),vec.a2hat)
  phi0<-vec.phi0(group,a1.hat,a2.hat,augMW)
  pred<-t(sapply(coef=coef,x=x,1:dim(x)[1],vec.pred.logit))
  alphahat<-sapply(A=group,pred=pred,1:length(group),vec.alphahat)
  phiest<-phi0+(group-p)*alphahat

  se.augMW<-sqrt(mean(phiest^2)/n)
  est_var<-as.numeric(mean(phiest^2)/n)

  # 95% CI:
  CI<-augMW+c(-1,1)*se.augMW*qnorm(1-alpha/2)
  # Wald test statistic:
  est_coef<-as.numeric(augMW)
  est_teststat<-(est_coef-0.5)/sqrt(est_var)
  # p-value Wald test:
  est_pval<-2*pnorm(-abs(est_teststat))

  res <- list(Coefficients= est_coef, Variance = est_var, Teststatistic = est_teststat, Pval=est_pval)

  return(res)
}

# Additional functions

## K-vermeulen webappendix
expit<-function(x){exp(x)/(1+exp(x))}
pseudo<-function(Y){
  I<-matrix(rep(Y,length(Y)),ncol=length(Y),byrow=TRUE)
  I<-ifelse(I<Y,1,ifelse(I==Y,0.5,0))
  return(t(I))
}
vec.a1hat<-function(A,pseudo.Y,i){
  sum(A[-i]*pseudo.Y[i,-i])/((sum(A)/length(A))*(length(A)-1))
}
vec.a2hat<-function(A,pseudo.Y,i){
  sum((1-A[-i])*pseudo.Y[-i,i])/((sum(1-A)/length(A))*(length(A)-1))
}
vec.phi0<-function(A,a1.hat,a2.hat,est){
  p<-sum(A)/length(A)
  (1-A)/(1-p)*a1.hat+A/p*a2.hat-2*est
}

vec.pred.logit<-function(coef,x,i){
  expit(coef[1]+t(-x[i,]+t(x[,]))%*%coef[2:(dim(x)[2]+1)])#expit
}
vec.alphahat<-function(A,pred,i){
  p<-sum(A)/length(A)
  sum(pred[i,-i]/(1-p)-pred[-i,i]/p,na.rm=TRUE)/(length(A)-1)
}

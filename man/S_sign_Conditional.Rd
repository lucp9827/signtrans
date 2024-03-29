% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S_sign_Conditional.R
\name{S_sign_Conditional}
\alias{S_sign_Conditional}
\title{Logistic regression model for the conditional S-null hypotheses}
\usage{
S_sign_Conditional(formula, data)
}
\arguments{
\item{formula}{is the conditional logistic regression model to fit (PO ~ group + auxiliary variables)}

\item{data}{is a dataframe with outcome variable PO in first column, grouping variable in the second column and auxiliary variables in the following columns}
}
\value{
list object with 1) fitted logistic regression model 2) estimated logistic regression coefficients 3) estimated variance-covariance matrix 4) Teststatistic 5) p-value
}
\description{
Logistic regression model for the conditional S-null hypotheses
}

#' Function to apply S-sign methods

#' @param formula is the model to be fitted
#' @param Method can be defined as 'Marginal', 'Conditional', 'RI' or 'All' (default is 'All')
#' @param db is a dataframe with raw count of 1 taxon (column 1) and the average reference frame (column 2)
#' @param startdata is a phyloseq object with in the sample data only the necessary variables to take into account (first sample variable should be group)

#' @import phyloseq
#' @export

#' @return list of results: S_sign_Marginal & S_sign_Conditional --> 1) logistic regression fit 2) estimated coefficients 3) estimated variance covariance matrix 4) Teststatistic 5) p-value AND S_sign_RI --> 1) estimated coefficients 2) estimated variance covariance matrix 3) Teststatistic 4) p-value

S_sign <-function(formula, Method="All", db, startdata, ...) {

  # Compute difference (taxa count - ref count)

  DIF <- Comp.Diff(db)

  # Compute Pseudo observations

  PO <- Comp.Pseudo.Obs(db,method='SIGN')

  # Define dataframe with outcome variable in 1st column, group variable in second column, and auxiliary variables in following columns

  data <- data.frame(PO=PO,sample_data(startdata))


  # Define model matrix

  mf <- model.frame(formula = formula, data=data)
  x <- model.matrix(attr(mf,"terms"),data=mf)
  y <- model.response(mf)

  # Apply method

  if (Method=='Marginal'){

    res<- S_sign_Marginal(data)
  }

  if (Method == "Conditional"){

    res<-S_sign_Conditional(formula,data)
  }

  if (Method == 'RI'){


    res <- S_sign_RI(formula,data)
  }

  if (Method == 'All'){

    # Marginal

    res_Marginal <- S_sign_Marginal(data)

    # Conditional

    res_Conditional <- S_sign_Conditional(formula,data)

    # Regression Imputation

    res_RI <- S_sign_RI(formula,data)

    # All results
    res <- list(Marginal = res_Marginal,Conditional = res_Conditional, RI =  res_RI)
  }


  return(res)
}



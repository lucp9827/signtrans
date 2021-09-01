#' Computes Pseudo Observations for the S-sign transformation and the ratio for the R-sign transformations.

#' @param db is a dataframe, with in the first column (1) OTU count and the second column the (average of the) reference frame.
#' @param method is a string which defines which sign transform is need, SIGN or RANK

#' @return for SIGN transforms is the Pseudo observations (based on the difference) and for RANK transforms is the Ratio
#' @export

Comp.Pseudo.Obs<-function(db,method) {

  if (method=='SIGN'){

    DIF<-Comp.Diff(db)

    # Computes Pseodo observations from the difference, which will be used as outcome variable for a logistic regression model
    PO<-(DIF<0)+0.5*(DIF==0)

    return(PO)}

  if (method=='RANK'){

    # Computes the Ratio, which will be used as outcome variable for a PIM model (pim function computes Pseudo observations)
    RATIO<- Comp.Ratio(db)

    return(RATIO)
  }
}

#' Computes differences between two columns/ return: difference

#' @param db is a dataframe, with in the first column (1) OTU count and the second column the (average of the) reference frame
#' @return difference between two columns (col1-col2)
#' @export

Comp.Diff<-function(db) {
  DIFC<-db[,1]-db[,2]
  return(DIFC)
}

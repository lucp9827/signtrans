#' Computes ratio between two columns and avoids inf values/ return: Ratio

#' @param db is a dataframe, with in the first column (1) OTU count and the second column the (average of the) reference frame.
#' If the reference frame = 0, the ratio is assigned a very large value to avoid inf values. Else the ratio is calculated by dividing the OTU count (col 1) by the (average of the) reference frame (col 2)

#' @return is the ratio (col1 /col2)
#' @export

Comp.Ratio<-function(db) {
  RATIO<-ifelse(db[,2]==0,999999,db[,1]/db[,2])
  return(RATIO)
}

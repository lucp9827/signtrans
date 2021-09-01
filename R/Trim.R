#' Trims OTU's that were present in at most 'minReads' sample(s) and with a prevalence less than 'minPrev'
#' Prevalence is the fraction of samples in which an OTU is observed at least `minReads` times


#' @param obj is a phyloseq object or (count) matrix
#' @param minReads is the minimum times an OTU was observed over all samples | Default=1
#' @param minPrev is the minimum fraction of samples in which an OTU is observed at least `minReads` times | Default=0.05

#' @return trimmed phyloseq object or (count)matrix
#' @import phyloseq
#' @export
#'
Trim <- function(obj, minReads = 1L, minPrev = 0.05) {

  if (class(obj) == "phyloseq") {
    taxRows <- taxa_are_rows(obj)
    if (!taxRows) {
      obj <- t(obj)
    } else {
    }
    otuTab <- as(otu_table(obj), "matrix")
  } else {
    otuTab <- obj
  }  # END - ifelse: obj is *phyloseq* or just *matrix*

  ## sort OTU first by prevalence, and then by total reads per OTU
  prevalence <- rowMeans(otuTab >= minReads)

  ## Will only keep OTU that appear in more than ('minPrev'*100)% of samples

  indOTUs2Keep <- (prevalence >= minPrev)

  if (class(obj) == "phyloseq") {
    obj = prune_taxa(obj, taxa = indOTUs2Keep)
    return(obj)
  } else {
    return(otuTab[indOTUs2Keep, ])
  }
}

#' Function to select taxa for the reference frame and compute the average

#' @param data is a phyloseq object of the raw trimmed counts
#' @param threshold to select reference taxa for the reference frame

#' @return a list object with 1) phyloseq object with counts of the non reference taxa 2) vector of the average of the reference frame

#' @import phyloseq
#' @import dacomp
#' @export
Select_Ref_frame <- function(data, threshold, stat=mean, nr=30){

# Convert otu table to a matrix for dacomp.select_references

counts<-as(otu_table(data), "matrix") # input dacomp.select_references, samples HAVE to be in rows!!

# Selection reference frame ~ Dacomp

result.selected.references <- dacomp.select_references(counts,median_SD_threshold=threshold,verbose = T)# Verbose = TRUE: prints computation progress

# Define counts of reference frame

ref_counts<-prune_taxa(colnames(counts)[result.selected.references$selected_references[1:nr]],data)

# Compute mean number of counts in the reference frame (must be postive)

ref_mean  <- apply(data.frame(otu_table(ref_counts)),1,stat)

# Define count table without reference frame

data_final<-prune_taxa(colnames(counts)[-result.selected.references$selected_references[1:nr]],data)

return(list(data_final = data_final,ref_mean =ref_mean))

}

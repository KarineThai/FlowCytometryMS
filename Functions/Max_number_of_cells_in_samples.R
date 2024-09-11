Max_number_of_cells_in_samples=function(x){
  #' Return the maximum number of cells contained in all samples
  #' @x an expression matrix created from a flowset
  nrow_in_df=c()
  for (i in 1:length(x)){
    nrow_in_df[[i]]=nrow(x[[i]])
  }
  return(max(as.vector(unlist(nrow_in_df))))
}
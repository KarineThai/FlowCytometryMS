add.IDcell = function(x) {
  #'
  #'Add cell ID to a lsit of dataframes
  #'
  #'@param x List of dataframes per sample containing expression matrices
  #'
  for (i in 1:length(x)){
    x[[i]]$Cell_ID=1:nrow(x[[i]])
  }
  return(x)
}

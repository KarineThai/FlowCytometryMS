get_best_result = function(caret_fit) {
  #'
  #'@param caret_fit A trained fit using caret package
  #'
  
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}
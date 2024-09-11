ApplyWilcox.test=function(df,feature, col){
  #'
  #'@param df df to use with proportions
  #'@param feature column containing two clinical groups
  #'@param col vector of columns corresponding to clusters
  #'
  #'
  source('Functions/makeStars.R')
  # variable = colnames(df)[col]
  pvals = c()
  for(var in col){
    res = wilcox.test(as.numeric(df[,var])~df[,feature], data = df, var.equal=T)
    pvals=c(pvals,res$p.value)
  }
  out = data.frame(cbind(pvals,variable))
  out[,1]=as.numeric(out[,1])
  out[,'p.adj']=p.adjust(out[,'pvals'], method='fdr', n=length(out[,'pvals']))
  out[,3]=as.numeric(out[,3])
  out[,'variable']=factor(out[,'variable'],out[order(-out[,'pvals']),"variable"])
  out[,'p.signif']=makeStars(out[,'p.adj'])
  print(head(out[order(out[,'pvals']),]))
  return(out)
}

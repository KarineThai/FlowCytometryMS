#Fonction pour tester diff'erent nombres de clusters FlowSOM et donner le graphique de heatmap
IterationNClusFlowSOM = function(ff, markers, nMeta = c(5,10,15,20)){
  #'
  #' @param ff The flowframe to use to generate the flowSOM
  #' @param colsToUse Parameters to use for FlowSOM (columns of the flowframe)
  #' @param nClus Number of meta Clusters to try
  #' @param fs flowSet or flowFrame to apply the FlowSOM on
  #' @param plotdir directory to write plots in and prefix for save plots before NClusters.pdf
  #'
List_FlowSOM = c()
for (n in nMeta) {
  List_FlowSOM[[as.character(n)]] = FlowSOM(input = ff,
                scale = T,
                compensate=F,
                transform = F,
                colsToUse = markers,
                seed = 1,
                nClus = n,
                xdim = 10, ydim = 10)
}
return(List_FlowSOM)
}

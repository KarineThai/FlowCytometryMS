StackedHistrogramsPerMarker = function(df, colCluster, markers, group) {
  #'@param df Dataframe containing expression values per marker
  #'@param colCluster Column in df containing the cluster numbers
  #'@param markers markers to plot
  #'@param group Column containing the group that will be used to color the curves
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(ggplot2, ggpubr, ggridges,tidyverse,dplyr)
  set.seed(123)
  color_clusters <- c(RColorBrewer::brewer.pal(12, 'Paired'),RColorBrewer::brewer.pal(8, 'Dark2'))
  cols = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#727272")
  shuffled_colors <- sample(color_clusters)
  plots = list()
  for (ma in markers) {
    p = ggplot(df, aes_string(x = ma, y = colCluster, color = group, fill = group)) + 
      geom_density_ridges(alpha = 0.3, rel_min_height = 0.01, scale = 4) + theme(axis.title.y = element_blank())+ ylab("") +
      scale_color_manual(values = cols) +
      scale_fill_manual(values = cols)+
      theme_minimal() + theme(text = element_text(size=8))
    plots[[ma]]=p 
  }
  return(plots)
}

GetPercentPositive3mark = function(data, id, positivitytable, markercol, posvaluecol, clustercol) {
  #' 
  #' @data The expression table
  #' @id Name of column containing sample ID in data
  #' @positivitytable A table containing positivity tresholds for each marker
  #' @markercol The name of the column containing marker names in positivitytable
  #' @posvaluecol The name of the column containing positive value tresholds stored in positivitytable
  #' @clustercol The name of the column containing cluster labels in data
  #' 
  #' 
  #' 
  library(data.table)
  library(textshape)
  
  result = data %>%
    group_by(!!sym(id), !!sym(clustercol)) %>%
    summarise(
      !!sym(paste0(positivitytable[1, markercol])) := mean(get(paste0(positivitytable[1, markercol])) > as.numeric(positivitytable[1, posvaluecol])) * 100,
      !!sym(paste0(positivitytable[2, markercol])) := mean(get(paste0(positivitytable[2, markercol])) > as.numeric(positivitytable[2, posvaluecol])) * 100,
      !!sym(paste0(positivitytable[3, markercol])) := mean(get(paste0(positivitytable[3, markercol])) > as.numeric(positivitytable[3, posvaluecol])) * 100
    )

      #sym(positivitytable[1,markercol]) = mean(sym(positivitytable[1,sym(markercol)]) > positivitytable[1,sym(posvaluecol)]) * 100, sym(positivitytable[2,markercol]) = mean(sym(positivitytable[2,sym(markercol)]) > positivitytable[2,sym(posvaluecol)]) * 100, sym(positivitytable[3,markercol]) = mean(sym(positivitytable[3,sym(markercol)]) > positivitytable[3,sym(posvaluecol)]) * 100
  
  m <- data.frame(melt(data.table(result), id.vars = c(id, clustercol),measure.vars = 3:ncol(result), variable.name='Marker',value.name = 'Pct'))
  
  m$PctMarkerInCluster=paste(m[,'Marker'],m[,clustercol], sep='_')
  m=m[,c(id, "Pct", "PctMarkerInCluster")] #Enlever les colonnes marqueur et cluster puisque l'on vient de créer une nouvelle colonne qui merge les deux
  m=reshape(m, idvar = id, timevar = 'PctMarkerInCluster', direction='wide') #Obtenir une longue table où chaque colonne est le MFI dechaque marqueur dans chaque cluster
  m=column_to_rownames(m, loc = 1)
  return(m)
}

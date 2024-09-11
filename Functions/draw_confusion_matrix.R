draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  #par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n', frame.plot = F)
  title('CONFUSION MATRIX', cex.main=2)
  #405b8e
  # create the matrix 
  rect(125, 430, 215, 370, col='#294e85')
  text(170, 435, 'HC', cex=1.6)
  rect(225, 430, 315, 370, col='#cfd6e3')
  text(270, 435, 'MS', cex=1.6)
  text(100, 370, 'Predicted', cex=1.8, srt=90, font=2)
  text(220, 450, 'Actual', cex=1.8, font=2)
  rect(125, 305, 215, 365, col='#cfd6e3')
  rect(225, 305, 315, 365, col='#294e85')
  text(115, 400, 'HC', cex=1.6, srt=90)
  text(115, 335, 'MS', cex=1.6, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(170, 400, res[1], cex=2, font=2, col='white')
  text(170, 335, res[2], cex=2, font=2, col='black')
  text(270, 400, res[3], cex=2, font=2, col='black')
  text(270, 335, res[4], cex=2, font=2, col='white')
  
  # add in the specifics 
  # plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n', cex.main = 2, frame.plot = F)
  # text(10, 85, names(cm$byClass[1]), cex=1.5, font=2)
  # text(10, 65, round(as.numeric(cm$byClass[1]), 3), cex=1.5)
  # text(35, 85, names(cm$byClass[2]), cex=1.5, font=2)
  # text(35, 65, round(as.numeric(cm$byClass[2]), 3), cex=1.5)
  # text(60, 85, names(cm$byClass[3]), cex=1.5, font=2)
  # text(60, 65, round(as.numeric(cm$byClass[3]), 3), cex=1.5)
  # text(85, 85, names(cm$byClass[4]), cex=1.5, font=2)
  # text(85, 65, round(as.numeric(cm$byClass[4]), 3), cex=1.5)
  # # 
  # # add in the accuracy information 
  # text(30, 30, names(cm$overall[1]), cex=1.8, font=2)
  # text(30, 5, round(as.numeric(cm$overall[1]), 3), cex=1.8)
  # text(70, 30, names(cm$overall[2]), cex=1.8, font=2)
  # text(70, 5, round(as.numeric(cm$overall[2]), 3), cex=1.8)
}

calc.acc = function(actual, predicted) {
  #' @actual testdata$GroupToTest
  #' @predicted caret prediction of model applied on test data
  mean(actual == predicted)
}
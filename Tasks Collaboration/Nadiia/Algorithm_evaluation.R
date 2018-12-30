library(caret)

create_conf_matrix <- function(refLabels, predictLabels, positiveLabel){
  conf_matrix <- confusionMatrix(
    refLabels, # reference labels
    predictLabels, # predicted labels
    positive = positiveLabel, # label that corresponds to a "positive" results (optional)
    dnn = c("actual", "predicted") # names of the confusion matrix dimensions  (optional)
  )
  return (conf_matrix)
}

get_evaluation <- function(refLabels, predictLabels, positiveLabel){
  conf_matrix <- create_conf_matrix(refLabels, predictLabels, positiveLabel)
  conf_matrix
  print(conf_matrix$overall["Accuracy"])
  print(conf_matrix$byClass["Sensitivity"])
  print(conf_matrix$byClass["Specificity"])
  ## todo add another evaluations
}
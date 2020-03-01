#' @title OASISAD evaluation function
#' @description Evaluate performance of segmentation models based on their outputs
#' @param pred a vector includes predicted labels from model.
#' @param truth a vector includes true labels. Please check labels' names in pred and truth are same.
#' @param pos_l a value indicates the positive label. If \emph{NULL}, default will set last value from \emph{table} function as positive label.
#' @return a table includes evaluation metrics.
#' @export

oasisad_eva <- function(pred,
                       truth,
                       pos_l = NULL){
  #number of subject
  n.obs <- length(pred)

  if(!is.null(pos_l)){
    truth <- truth == pos_l
    pred <- pred == pos_l
  }

  # Confusion matrix
  x <- table(truth, pred) # confusion matrix
  TP <- x[2,2] ## if "1" is positive
  TN <- x[1,1] ## if "0" is negative
  FP <- x[1,2]
  FN <- x[2,1]

  # Metrics table
  accuracy = (TP+TN)/n.obs # accuracy
  if((FP+TP)!=0) precision = TP/(FP+TP) else precisoin = 0 #positive predicted value
  if((FN+TP)!=0) recall = TP/(FN+TP) else  recall = 0 #true positive rate
  if((TN+FP)!=0) fpr = FP/(TN+FP) else fpr = 0 #false positive rate
  if((2*TP + FP +FN)!=0) Dsc = 2*TP/(2*TP + FP +FN) else Dsc = 0 #dice similarity score i.e. F1-score
  measure = c(accuracy, precision, recall, fpr, Dsc)
  names(measure) = c("Accuracy", "Precision", "Recall",'fpr',"Dsc")

  return(measure)
}

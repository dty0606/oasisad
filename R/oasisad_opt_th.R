#' @title OASISAD optimal threshold selection function
#' @description This function will use an input cutoff list to select the optimal
#' cutoff with metric DSC(F1-score) based on validation data
#' @param probs Probability map
#' @param gs Gold standard of input voxels
#' @param cutoff A numeric vector includes threshold candidates
#' @return Refined probability map

opt_thre <- function(probs,
                     gs,
                     cutoff){
    DSC <- vector()
    gold <- factor(gs == 1, levels = c(TRUE,FALSE))
    # DSC of each threshold candidate
    for (j in seq(length(cutoff))) {
      c <- cutoff[j]
      preds <- factor(probs >= c, levels = c(TRUE,FALSE))
      conf_mat <- table(gold, preds)
      DSC[j] <- (2*conf_mat[1,1])/(2*conf_mat[1,1]+conf_mat[1,2]+conf_mat[2,1])
    }
    return(cutoff[which.max(DSC)])
}

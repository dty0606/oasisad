#' @title OASISAD model
#' @description  The input should be OASISAD data list,
#' the function will train the model with training and vailidaion data,
#' then use the testing data to evaluatoin performance
#' @param train_df A data list from oasisad_df function which inlcudes training samples informatin.
#' If neighbor refinement function will be used, the list should include segmentation and
#' white matter probability map for each training subject
#' @param test_df A data list from oasisad_df function which inlcudes testing samples informatin.
#' If neighbor refinement function will be used, the list should include segmentation and
#' white matter probability map for each training subject
#' @param valid_df A data list from oasisad_df function which inlcudes validation samples informatin.
#' If neighbor refinement function will be used, the list should include segmentation and
#' white matter probability map for each training subject. If it is NULL, optimal threshold
#' algorithm will be used to calculate threshold
#' @param M1 A boolean indicates using full model 'M1' or reduced model 'M2',
#' default is reduced model
#' @param refine A boolean incicates whether use OASISAD refinement function,
#' to refine probability map from logistic regression model
#' @param neighbor A boolean incicates whether use neighbor refinement function,
#' to refine probability map from logistic regression model. If true, segmentation information and
#' white matter probability of brain is needed
#' @param wm_label White matter label in segmentation input
#' @param re_value A numeric value will be used in neighor refinement functoin to
#' refine a voxel's probability of being White matter hyperintensity
#' @return OASISAD model results
#' @export
#' @importFrom stats glm family predict binomial

oasisad_model <- function(train_df,
                          test_df,
                          valid_df = NULL,
                          M1 = FALSE,
                          refine = FALSE,
                          neighbor = FALSE,
                          wm_label = NULL,
                          re_value = NULL) {

    #train, val, test data
    for(i in 1:length(train_df)){
      if(i == 1){
        train <- train_df[[i]]$data
      } else {
        train <- rbind(train, train_df[[i]]$data)
      }
    }
    #
    if(!is.null(valid_df)){
      for(i in 1:length(valid_df)){
        if(i == 1){
          valid <- valid_df[[i]]$data
        } else {
          valid <- rbind(valid, valid_df[[i]]$data)
        }
      }
    }

    for(i in 1:length(test_df)){
      if(i == 1){
        test <- test_df[[i]]$data
      } else {
        test <- rbind(test, test_df[[i]]$data)
      }
    }
    #masks for refinement

    if(M1){
      model <- glm(GoldStandard ~ flair*flair.1
                + flair*flair.2 + t1*t1.1 + t1*t1.2, family=binomial, data=train)
    }else{
      model <- glm(GoldStandard ~ flair+t1, family=binomial, data=train)
    }

    # if there is validation data, optimal threshold will be calculated
    if(!is.null(valid_df)){
      # the default cutoff_list is seq(0,1,0.01)
      probs <- predict(model, valid, type = "response")
      if(refine){
        for(i in 1:length(valid_df)){
          sub <- valid_df[[i]]
          wm_mask <- sub$wm_mask
          seg_mask <- sub$seg_mask
          indx <- sub$data$indx
          indx3d <- sub$data[c("axial", "coronal", "sagittal")]
        }
        probs <- oasis_refine(probs,
                              neighbor = neighbor,
                              seg = seg_mask,
                              wm = wm_mask,
                              wm_label = NULL,
                              re_value = NULL,
                              indx = indx,
                              indx3d = indx3d)
      }
      #gold standard
      for(i in 1:length(valid_df)){
        if(i == 1){
          gs <- valid_df[[i]]$data$GoldStandard
        } else {
          gs <- rbind(gs, valid_df[[i]]$data$GoldStandard)
        }
      }
      #optimal threshold will use DSC(F1-score) as metric
      cutoff <- seq(0,1,0.01)
      opt_cut <- opt_thre(probs,gs=gs, cutoff)
    }

  ## Apply to test data
  probs_list <- list()
  for(i in 1:length(test_df)){
    probs <- predict(model, test, type = "response")
    if(refine){
    sub <- test_df[[i]]
    wm_mask <- sub$wm_mask
    seg_mask <- sub$seg_mask
    indx <- sub$data$indx
    indx3d <- sub$data[c("axial", "coronal", "sagittal")]
    probs <- oasis_refine(probs,
                          neighbor = neighbor,
                          seg = seg_mask,
                          wm = wm_mask,
                          wm_label = NULL,
                          re_value = NULL,
                          indx = indx,
                          indx3d = indx3d)
    }
    probs_list[[i]] <- probs
  }
  L <- list(coef = model$coefficients,
            cutoff = opt_cut,
            probs = probs_list
            )
}

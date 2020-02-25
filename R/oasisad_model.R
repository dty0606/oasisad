#' @title OASISAD image preprocessing function
#' @description  MRI image data preprocessing with multiple inputs
#' @param df A dataframe from oasisad_df
#' @param M1 A boolean indicates using full model 'M1' or reduced model 'M2',
#' default is reduced model
#' @param t1 Input T1 image
#' @param t2 Input T2 image
#' @param pd Input PD image
#' @param t1mask A boolean indicates whether use T1 as brain mask or not
#' @param brain_mask Input brain_mask, if null, a mask will be obtained by FSL
#' @param dir A user defined output
#' @param cores A number indicates how many cores used mclapply

oasis_ad <- function(train_df,
                      test_df,
                      valid_df = NULL,
                      threshold = NULL,
                      M1 = FALSE,
                      refine = FALSE,
                      brain_mask = NULL,
                      neighbor = FALSE,
                      seg = NULL,
                      wm = NULL,
                      wm_label = NULL,
                      re_value = NULL,
                      indx = NULL) {

    if(M1){
      M1 <- glm(GoldStandard ~ FLAIR*FLAIR_10
                + FLAIR*FLAIR_20 + T1*T1_10 + T1*T1_20, family=binomial, data=df)
    }else{
      M2 <- glm(GoldStandard ~ FLAIR+T1, family=binomial, data=df)
    }

    if
    ##idx
    idx.use = test$idx.use
    n.obs = nrow(test) # no. of observations in train
    probs = predict(model, newdata=test, type="response")
    nei_mat = as.matrix(test[,c("wm.pve", "nei1","nei2","nei3","nei4","nei5","nei6")])
    test$wm.correct = sapply_pb(1:n.obs, function(i) neighbor(nei_mat[i,],
                                                              seg.voxel = seg.voxel,
                                                              wm.pve=wm.pve,
                                                              gm.pve=gm.pve,
                                                              csf.pve=csf.pve))
    #different probability map
    probsr = probs^test$wm.correct
    probss = smoo(probs,brain_mask,idx.use)
    probsrs = smoo(probsr,brain_mask,idx.use)
    probssr = probss^test$wm.correct
    prob = list(probs = probs, probsr = probsr,
                probss = probss, probsrs = probsrs, probssr = probssr)

    ##save all probs
    save(prob,file = file.path(subout,paste0(sub,'_', outname,'_prob.RData')))

    # ##put in test dataset
    # test$probs = probs
    # test$probsr = probsr
    # test$probss = probss
    # test$probsrs = probsrs
    # test$probssr = probssr
    # ##gold standard
    # actuals = test$GoldStandard
    ##if mingjie is true
    if(is.mingjie){
      ##transform to binary mask, mingjie
      minjie <- mingjie > 0
      # confusion.matrix = table(actuals, mingjie) ## confusion matrix
      # print(confusion.matrix)
      # metric.mingjie = round(metric(confusion.matrix, n.obs),4)
      save(minjie,file=file.path(subout,paste0(sub,'_minjie_seg.RData')))
    }
    # if(auc){
    #   ## calculate auc value
    #   auc = pROC::auc(actuals,probs)
    #   aucr = pROC::auc(actuals,probsr)
    #   aucs = pROC::auc(actuals,probss)
    #   aucrs = pROC::auc(actuals,probsrs)
    #   aucsr = pROC::auc(actuals,probssr)
    #   ## plot ROC
    #   perf = list()
    #   for(i in 1:length(prob)){
    #     pred = prediction(prob[[i]],actuals)
    #     perf[[i]] = performance(pred, "tpr","fpr")
    #   }
    #   perf[[length(prob)+1]] = c(auc,aucr,aucs,aucrs,aucsr)
    # }
    #cutoff start from here, loop over cutoff
    # for(i in seq(length(cutoff_list))){
    #   #current cutoff
    #   cutoff = cutoff_list[i]
    #   predssr = floor(probssr + (1-cutoff))
    #   predsrs = floor(probsrs + (1-cutoff))
    #   predss = floor(probss + (1-cutoff))
    #   predsr = floor(probsr + (1-cutoff))
    #   preds = floor(probs + (1-cutoff)) ## classified binary values
    #
    #   #CONFUSION
    #   confusion.matrix = table(actuals, preds) ## confusion matrix
    #   sprintf("Cutoff = %f", cutoff)
    #   print(confusion.matrix)
    #   if(auc) metric.n = c(metric(confusion.matrix, n.obs), auc) else
    #     metric.n = metric(confusion.matrix, n.obs)
    #
    #   confusion.matrix = table(actuals, predsr) ## confusion matrix
    #   print(confusion.matrix)
    #   if(auc) metric.r = c(metric(confusion.matrix, n.obs), aucr) else
    #     metric.r = metric(confusion.matrix, n.obs)
    #
    #   confusion.matrix = table(actuals, predss) ## confusion matrix
    #   print(confusion.matrix)
    #   if(auc) metric.s = c(metric(confusion.matrix, n.obs), aucs) else
    #     metric.s = metric(confusion.matrix, n.obs)
    #
    #   confusion.matrix = table(actuals, predsrs) ## confusion matrix
    #   print(confusion.matrix)
    #   if(auc) metric.rs = c(metric(confusion.matrix, n.obs), aucrs) else
    #     metric.rs = metric(confusion.matrix, n.obs)
    #
    #   confusion.matrix = table(actuals, predssr) ## confusion matrix
    #   print(confusion.matrix)
    #   if(auc) metric.sr = c(metric(confusion.matrix, n.obs), aucsr) else
    #     metric.sr = metric(confusion.matrix, n.obs)
    #
    #   ##save metric
    #   metric_bind = rbind(metric.n, metric.r, metric.s, metric.rs, metric.sr)
    #   if(auc) colnames(metric_bind)[5] = "AUC"
    #
    #   ##for loop write mask
    #   mask_list = list(preds,predsr,predss,predsrs,predssr)
    #   for(j in seq(length(mask_list))){
    #     pred = mask_list[[j]]
    #     ##output mask
    #     mask_out <- niftiarr(seg.voxel, 0)
    #     mask_out[test$idx.use[pred==1]] = 1
    #     writenii(mask_out,file.path(subout,outname,paste0(sub,'_', outname,'_', cutoff)))
    #   }
    #
    #   #save metric of each cutoff to metric list
    #   metric_list[[i]] <- round(metric_bind,4)
    #   }
  }
  if(oasis){
    ##idx
    idx.use = test$idx.use
    n.obs = nrow(test) # no. of observations in train
    probs = predict(model, newdata=test, type="response")
    save(probs,file = file.path(subout,paste0(sub,'_', outname,'_probs.RData')))
    # ##confusion
    # actuals = test$GoldStandard
    # ## calculate auc value
    # if(auc){
    #   perf = list()
    #   auc = pROC::auc(actuals,probs)
    #   pred = prediction(probs,actuals)
    #   perf = performance(pred, "tpr","fpr")
    # }
    # for(i in seq(length(cutoff_list))){
    #   cutoff = cutoff_list[i]
    #   preds = floor(probs + (1-cutoff)) ## classified binary values
    #   confusion.matrix = table(actuals, preds) ## confusion matrix
    #   print(confusion.matrix)
    #   metric_o = c(metric(confusion.matrix, n.obs), auc)
    #   names(metric_o)[5] = 'AUC'
    #
    #   #write mask
    #   mask_out <- niftiarr(seg.voxel, 0)
    #   mask_out[as.numeric(test$idx.use[preds==1])] <- 1
    #   writenii(mask_out, file.path(subout,outname,paste0(sub,'_', outname,'_',cutoff)))
    #
    #   #save metric of each cutoff to metric list
    #   metric_list[[i]] <- round(metric_o,4)
    # }
  }
  if(mimosa){
    ##idx, for mimosa, fill overlap to our chosen voxels list
    idx = test$idx
    test_data = test$data
    n.obs = length(idx) # no. of observations we will use
    probs = predict(model, newdata = test_data, type = 'response')
    #save(probs,file = file.path(subout,paste0(sub,'_', outname,'_probs.RData')))
    ##gold
    actuals = test$gold
    ## calculate auc value
    ##probs and actual use for auc
    probs_use = probs[test_data$idx.use %in% idx]
    probs_full = rep(0,n.obs)
    probs_full[idx %in% test_data$idx.use] =  probs_use
    save(probs_full,file = file.path(subout,paste0(sub,'_', outname,'_probs.RData')))
    #   perf = list()
    #   auc = pROC::auc(actuals,probs_full)
    #   pred = prediction(probs_full,actuals)
    #   perf = performance(pred, "tpr","fpr")
    # for(i in seq(length(cutoff_list))){
    #     cutoff = cutoff_list[i]
    #     ##create preds as 0 vector fill with overlap voxels
    #     preds_mimosa = floor(probs_use + (1-cutoff)) ## classified binary values
    #     preds = rep(0,length(idx))
    #     preds[idx %in% test_data$idx.use] = preds_mimosa
    #     #confusion
    #     confusion.matrix = table(actuals, preds) ## confusion matrix
    #     print(confusion.matrix)
    #     metric_m = c(metric(confusion.matrix, n.obs), auc)
    #     names(metric_m)[5] = 'AUC'
    #
    #     #write mask
    #     mask_out <- niftiarr(brain_mask, 0)
    #     mask_out[as.numeric(idx[preds==1])] <- 1
    #     writenii(mask_out, file.path(subout,outname,paste0(sub,'_', outname,'_',cutoff)))
    #
    #     #save metric of each cutoff to metric list
    #     metric_list[[i]] <- round(metric_m,4)
    # }
  }
  # save(metric_list, file = file.path(subout,outname,paste0(sub,'_', outname,'_metric.RData')))
  # return(metric_list)
}

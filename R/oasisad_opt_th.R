###use training to obtain optimal threshold
opt_thre <- function(M1, ,
                     brain_mask, dir_seg, terry = F){
  # Matrix for keeping DSC for each training subject
  DSC_mat = matrix(NA, nrow = length(train_sub), ncol = length(cutoff_list))
  if(terry){
    DSC_matr = matrix(NA, nrow = length(train_sub), ncol = length(cutoff_list))
    DSC_mats = matrix(NA, nrow = length(train_sub), ncol = length(cutoff_list))
    DSC_matrs = matrix(NA, nrow = length(train_sub), ncol = length(cutoff_list))
    DSC_matsr = matrix(NA, nrow = length(train_sub), ncol = length(cutoff_list))
  }
  for(i in seq(length(train_sub))){
    current_sub = mydata_train %>% filter(id == train_sub[i])
    if(terry){
      ##load ero_brain_mask and build empty mask
      load(file.path(dir_seg, paste0(train_sub[i],'_oasisframe_t2coreg_full.RData')))
      brain_mask <- dataframe1$ero_brain_mask
      mask_nifti <- niftiarr(brain_mask, 0)
      ##load segmentation information
      seg.voxel = readnii(file.path(dir_seg,paste0(train_sub[i],'_flaircoreg_seg.nii.gz')))
      csf.pve = readnii(file.path(dir_seg,paste0(train_sub[i],'_flaircoreg_pve_0.nii.gz')))
      gm.pve = readnii(file.path(dir_seg,paste0(train_sub[i],'_flaircoreg_pve_1.nii.gz')))
      wm.pve = readnii(file.path(dir_seg,paste0(train_sub[i],'_flaircoreg_pve_2.nii.gz')))
      ##idx
      idx.use = current_sub$idx.use
      n.obs = nrow(current_sub) # no. of observations in train
      nei_mat = as.matrix(current_sub[,c("wm.pve", "nei1","nei2","nei3","nei4","nei5","nei6")])
      current_sub$wm.correct = sapply_pb(1:n.obs, function(i) neighbor(nei_mat[i,],
                                                                       seg.voxel = seg.voxel,
                                                                       wm.pve = wm.pve,
                                                                       gm.pve = gm.pve,
                                                                       csf.pve = csf.pve))
    }
    # Probability map
    probs = predict(model, current_sub, type = "response")
    if(terry){
      probsr = probs^current_sub$wm.correct
      probss = smoo(probs,brain_mask,idx.use)
      probsrs = smoo(probsr,brain_mask,idx.use)
      probssr = probss^test$wm.correct
    }
    probs_map = probs
    if(terry){
      probsr_map = probsr
      probss_map = probss
      probsrs_map = probsrs
      probssr_map = probssr
    }
    # DSC of each threshold candidate
    for (j in seq(length(cutoff_list))) {
      preds = factor(probs_map >= cutoff_list[j], levels=c(TRUE,FALSE))
      conf_mat = table(factor(current_sub$GoldStandard,levels=c(TRUE,FALSE)), preds)
      DSC_mat[i,j] = (2*conf_mat[1,1])/(2*conf_mat[1,1]+conf_mat[1,2]+conf_mat[2,1])
      if(terry){
        #NNR
        predsr = factor(probsr_map >= cutoff_list[j], levels=c(TRUE,FALSE))
        conf_mat = table(factor(current_sub$GoldStandard,levels=c(TRUE,FALSE)), predsr)
        DSC_matr[i,j] = (2*conf_mat[1,1])/(2*conf_mat[1,1]+conf_mat[1,2]+conf_mat[2,1])
        #GFR
        predss = factor(probss_map >= cutoff_list[j], levels=c(TRUE,FALSE))
        conf_mat = table(factor(current_sub$GoldStandard,levels=c(TRUE,FALSE)), predss)
        DSC_mats[i,j] = (2*conf_mat[1,1])/(2*conf_mat[1,1]+conf_mat[1,2]+conf_mat[2,1])
        #NNR+GFR
        predsrs = factor(probsrs_map >= cutoff_list[j], levels=c(TRUE,FALSE))
        conf_mat = table(factor(current_sub$GoldStandard,levels=c(TRUE,FALSE)), predsrs)
        DSC_matrs[i,j] = (2*conf_mat[1,1])/(2*conf_mat[1,1]+conf_mat[1,2]+conf_mat[2,1])
        #GFR+NNR
        predssr = factor(probssr_map >= cutoff_list[j], levels=c(TRUE,FALSE))
        conf_mat = table(factor(current_sub$GoldStandard,levels=c(TRUE,FALSE)), predssr)
        DSC_matsr[i,j] = (2*conf_mat[1,1])/(2*conf_mat[1,1]+conf_mat[1,2]+conf_mat[2,1])
      }
    }
  }

  if(terry){
    optimalr <- cutoff_list[which.max(apply(DSC_matr,2,mean))]
    optimals <- cutoff_list[which.max(apply(DSC_mats,2,mean))]
    optimalrs <- cutoff_list[which.max(apply(DSC_matrs,2,mean))]
    optimalsr <- cutoff_list[which.max(apply(DSC_matsr,2,mean))]
    return(c(optimalr, optimals, optimalrs, optimalsr))
  } else {
    optimal <- cutoff_list[which.max(apply(DSC_mat,2,mean))]
  }
}

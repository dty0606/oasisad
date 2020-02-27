#' @title OASISAD image preprocessing function
#' @description  MRI image data preprocessing with multiple inputs
#' @param flair Input FLAIR image
#' @param t1 Input T1 image
#' @param t2 Input T2 image
#' @param pd Input PD image
#' @param img_space An image to register and use for creating brain mask if needed.
#' If NULL, 'flair' image will be used in registration.
#' @param brain_mask Input brain_mask, if null, a mask will be obtained by FSL
#' @param dir A user defined output path for fslr segmentation
#' @param cores A number indicates how many cores used mclapply
#' @param verbose A boolean indicated whether output messages

oasisad_pre <- function(flair, #flair volume of class nifti
                        t1, # t1 volume of class nifti
                        t2 = NULL, # t2 volume of class nifti
                        pd = NULL, # pd volume of class nifti
                        img_space = NULL,
                        brain_mask = NULL,
                        segmentation = FALSE,
                        dir = NULL,
                        cores = 1, # number of cores used in mclapply,
                        verbose = NULL
){
  study <- list(flair = flair, t1 = t1, t2 = t2, pd = pd)
  # Remove null modality
  nulls <- sapply(study, is.null)
  study <- study[!nulls]

  #coregisters to a same space
  if(is.null(img_space)){
    img_space <- flair
  }
  #check whether img_space is in study or not
  temp <- sapply(study, identical, y = img_space)
  if(sum(temp) > 0){
    img_type <- names(study)[which(temp)]
  }
  study_temp <- study
  study_temp[[img_type]] <- NULL
  study_temp <- mclapply(study_temp, flirt, reffile = img_space, mc.cores = cores)
  study_temp[[img_type]] <- study[[img_type]]
  study <- study_temp
  rm(study_temp)

  #create brain mask
  if (verbose) {
    message("Running Brain Extraction Tool\n")
  }
  if (is.null(brain_mask)) {
      brain_mask <- fslbet_robust(img_space, remove.neck = T, correct = T, correction = "N4",
                                  recog = T, retimg = TRUE, verbose = F)
  }

  #fast segmention by FSL
  if(segmentation){
    fast_seg <- fast(file=brain_mask, outfile=dir,opts= "-N")
  }
  brain_mask <- check_nifti(brain_mask)
  brain_mask <- brain_mask > 0
  brain_mask <- datatyper(brain_mask, trybyte = TRUE)
  #mask input images with brain_mask
  study <- check_nifti(study)
  study <- mclapply(study, mask_img, mask = brain_mask, mc.cores = cores)
  study$brain_mask <- brain_mask

  ##return a list with the preprocessed images and a brain mask
  return(study)
}





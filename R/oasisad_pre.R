#' @title OASISAD image preprocessing function
#' @description  MRI image data preprocessing with multiple inputs
#' @param flair Input FLAIR image
#' @param t1 Input T1 image
#' @param t2 Input T2 image
#' @param pd Input PD image
#' @param t1mask A boolean indicates whether use T1 as brain mask or not
#' @param brain_mask Input brain_mask, if null, a mask will be obtained by FSL
#' @param dir A user defined output
#' @param cores A number indicates how many cores used mclapply

oasisad_pre <- function(flair, #flair volume of class nifti
                      t1, # t1 volume of class nifti
                      t2 = NULL, # t2 volume of class nifti
                      pd = NULL, # pd volume of class nifti
                      t1mask = FALSE, # Brain mask using T1
                      brain_mask = NULL,
                      dir = NULL,
                      cores = 1 # number of cores used in mclapply
){
  study <- list(flair = flair, t1 = t1, t2 = t2, pd = pd)
  # Remove null modality
  nulls <- sapply(study, is.null)
  study <- study[!nulls]
  if (verbose) {
    message("Running Brain Extraction Tool\n")
  }
  if (is.null(brain_mask)) {
    if(t1mask)
      brain_mask <- fslbet_robust(t1, remove.neck = T, correct = T, correction = "N4",
                                  recog = T, retimg = TRUE, verbose = F)
    else
      brain_mask <- fslbet_robust(flair, remove.neck = T, correct = T, correction = "N4",
                                  recog = T, retimg = TRUE, verbose = F)
  }

  #fast segmention by FSL
  fast_seg <- fast(file=brain_mask, outfile=paste(dir),opts= "-N")
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





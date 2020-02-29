#' @title OASIS Data Frame
#' @description This function creates data frame for OASIS models
#' @param flair Input FLAIR image
#' @param t1 Input T1 image
#' @param t2 Input T2 image
#' @param pd Input PD image
#' @param gold_standard gold standard lesion segmentation mask of class
#' \code{\link{nifti}}
#' #' @param preproc A boolean indicates whether to call
#' \code{\link{oasisad_pre}} function or not
#' and performs the necessary preprocessing steps for OASIS
#' @param brain_mask Input brain_mask, if null, a mask will be obtained by FSL
#' @param img_space An image to register, if NULL, 'flair' image will be used in registration.
#' @param neighbor A boolean indicates whether will use neighbor refinement function in model step.
#' If true, either prepare segmentation and white matter mask to input in this function or
#' this functoin will generate masks by \code{\link{fslr}}
#' @param wm_mask Input of white matter mask
#' @param seg_mask Input of segmentation mask
#' @param dir A user defined output path for fslr segmentation
#' @param eroder A boolean indicates whether should use \code{\link{fslerode}}
#' @param voxel_select A specifed level to remove voxels whose intensity under
#' @param normalize A boolean indicates whether to
#' perform z-score normalization of the image over the brain mask,
#' should be \code{TRUE} unless you train model
#' using an alternative normalization. Normoalization is a recommended step in image preprocessing.
#' @param image_sm A boolean indates whether to smooth images and used as predictors in model
#' @param slices vector of desired slices to train on, if \code{NULL}
#' then train over the entire brain mask
#' @param orientation string value telling which orientation the
#' training slices are specified in, can take the values of "axial",
#' "sagittal", or "coronal"
#' @param return_pre is a logical value that indicates whether
#' the preprocessed images should be returned
#' @param cores numeric indicating the number of cores to be used
#' @param verbose A boolean indicated whether output messages

oasisad_df <- function(flair, ##flair volume of class nifti
                        t1, ##t1 volume of class nifti
                        t2 = NULL, ##t2 volume of class nifti
                        pd = NULL, ##pd volume of class nifti
                        gold_standard = NULL, ##gold standard mask of class snifti
                        preproc = TRUE, ##option to preprocess the data
                        brain_mask = NULL, ##brain mask of class nifti
                        img_space = FALSE, ## input of image for registration
                        neighbor = TRUE,
                        wm_mask = NULL,
                        seg_mask = NULL,
                        dir = NULL,
                        eroder = TRUE,
                        voxel_select = NULL, ##a specifed level to remove voxels whose intensity under
                        normalize = TRUE, ##option to normalize
                        image_sm = TRUE, ## option to smooth image
                        slices = NULL, #slice vector
                        orientation = c("axial", "coronal", "sagittal"),
                        return_preproc = FALSE,
                        cores = 1,
                        verbose = TRUE
)
{

  check_nifti2 = function(x) {
    if (is.null(x)) {
      return(NULL)
    } else {
      return(check_nifti(x))
    }
  }

  #check input images are nifti
  flair <- check_nifti2(flair)
  t1 <- check_nifti2(t1)
  t2 <- check_nifti2(t2)
  pd <- check_nifti2(pd)

  #image preproceesing
  if (preproc == TRUE) {
    if (verbose) {
      message("OASISAD Preprocessing")
    }

    segmentation = FALSE
    if(neighbor){
      if(is.null(wm_mask) & is.null(seg_mask)){
        if (verbose) {
          message("No white matter and segmentation masks input, using fslr for creating masks")
        }
        segmentation = TRUE
      }

    }
    ## the image preproceesing
    preprocess <- oasisad_pre(flair = flair,
                                t1 = t1,
                                t2 = t2,
                                pd = pd,
                                img_space = img_space,
                                brain_mask = brain_mask,
                                segmentation = segmentation,
                                verbose = verbose,
                                cores = cores,
                                dir = dir)
    oasisad_study <- preprocess[c("flair","t1", "t2", "pd")]
    brain_mask <- preprocess$brain_mask
  } else {
    ## no preprocessing
    oasisad_study <- list(flair = flair, t1 = t1, t2 = t2, pd = pd)
  }

  # REMOVE NULL
  nulls <- sapply(oasisad_study, is.null)
  oasisad_study <- oasisad_study[!nulls]

  ###############################
  # Making brain mask if one not needed
  ###############################

  ##erode mask if needed
  if(eroder){
    if (verbose) {
      message("Eroding Brain Mask")
    }
    brain_mask <- fslerode(brain_mask,
                           kopts = "-kernel box 5x5x5",
                           retimg = TRUE)
  }


  # removing voxels below a certain quantile if needed
  if(!is.null(voxel_select)){
    top_voxels <- voxel_selection(flair = oasis_study$flair,
                                  brain_mask = brain_mask,
                                  cutoff = voxel_select)
    oasis_studyad$top_voxels <- top_voxels
    rm(top_voxels)
  }

  # the image normalization
  if (normalize == TRUE) {
    if (verbose) {
      message("Normalizing Images using Z-score")
    }
    oasisad_study <- lapply(oasisad_study, zscore_img,
                          mask = brain_mask,
                          margin = NULL)
  }

  # smoothing images
  if (image_sm == TRUE) {

    if (verbose) {
      message("Smoothing Images: width = 10")
    }

    # smooth the images using fslsmooth from the fslr package
    smooth_10 <- mclapply(oasisad_study, fslsmooth,
                          sigma = 10,
                          mask = brain_mask,
                          retimg = TRUE,
                          smooth_mask = TRUE,
                          mc.cores = cores)

    if (verbose) {
      message("Smoothing Images: width = 20")
    }
    smooth_20 <- mclapply(oasisad_study, fslsmooth,
                         sigma = 20,
                         mask = brain_mask,
                         retimg = TRUE,
                         smooth_mask = TRUE,
                         mc.cores = cores)
    oasisad_study <- c(oasisad_study, smooth_10, smooth_20)

    rm(list = c("smooth_10","smooth_20"))

  }

  # check gold_standard and attach to oasis_study dataframe
  gold_standard <- check_nifti2(gold_standard)
  oasisad_study$GoldStandard <- gold_standard

  #######################################
  # Make data.frame
  #######################################
  oasisad_data <- lapply(oasisad_study, c)
  oasisad_data <- as.data.frame(oasisad_data)
  rownames(oasisad_data) = NULL

  ######################
  # Keep index
  ######################
  indx <- niftiarr(brain_mask, 1)
  indx <- which(indx == 1, arr.ind = TRUE)
  orientations <- c("axial", "coronal", "sagittal")
  colnames(indx) <- orientations
  oasisad_data <- cbind(oasisad_data, indx)
  oasisad_data$indx <- c(1:length(brain_mask))

  ######################
  # If Keep Voxel Selection
  ######################
  if(!is.null(voxel_select)){
    oasisad_data <- oasisad_data[oasisad_data$top_voxels == 1, ]
    oasisad_data$top_voxels <- NULL
  }

  ######################
  # If select slices
  ######################
  if (!is.null(slices)) {
    orientation <- match.arg(orientation)
    oasisad_data <- oasisad_data[oasisad_data[, orientation] %in% slices, ]
  }

  ######################
  # output voxels within indices
  ######################
  oasisad_data <- oasisad_data[which(brain_mask != 0), ]

  ######################
  # create a list to save dataframe, wm_mask, seg_mask information
  ######################
  L <- list(data = NULL, wm_mask = NULL, seg_mask = NULL)
  L$data <- oasisad_data

  if(neighbor){
    if(is.null(wm_mask)){
      wm_mask <- readnii(paste0(dir,'_pve_2.nii.gz'))
    }
    if(is.null(seg_mask)){
      seg_mask <- readnii(paste0(dir,'_seg.nii.gz'))
    }
    L$wm_mask <- wm_mask
    L$seg_mask <- seg_mask
  }
  return(L)
}

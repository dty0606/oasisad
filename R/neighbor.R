#' Refine voxel's probability of being white matter hypeintensity by neighbor information
#'
#' @param x a dataframe includes a voxel's probability of being white matter and its nearest neighor voxels' vetorized indice.
#' @param seg.voxel a vector includes segmention information, for example, segmention by FSL.
#' @param wm.pve a probablity map of being white matter from segmentation tool such as FSL.
#' @param gm.pve a probablity map of being grey matter from segmentation tool such as FSL.
#' @param csf.pve a probablity map of being CSF from segmentation tool such as FSL.
#' @param wm_label a value indicates white matter label in input seg.voxel argument. Default is 3, by FSL.
#' @param fun a funtion to aggregate neighbor information, default is describe in OASIS-AD paper.
#' @return a value to refine probability map of being white matter hyperintensity

neighbor <- function(x,
                     seg.voxel,
                     wm.pve,
                     gm.pve,
                     csf.pve,
                     wm_label = NULL,
                     re_value = NULL){
  ## obtain neighbor information by indice
  nei_seg <- na.omit(seg.voxel[x[-1]])
  wmpve_nei <- na.omit(wm.pve[x[-1]])
  gmpve_nei <- na.omit(gm.pve[x[-1]])
  csfpve_nei <- na.omit(csf.pve[x[-1]])

  ## refined parameter calculation
  if(x[1] == 1){
    if(is.null(wm_label)){
        label = 3
      }
    if(sum(nei_seg == label) < length(nei_seg)){
      return(mean(wmpve_nei)) # if not surrounded by all white matter, return mean of wmpve
    } else {
      if(is.null(re_value)){
        return(10)
        } else {
          return(re_value)
        }# else return a large value (>1) to suppress its' probability as WMH
    }
  } else {
    return(1) # if the voxel is not classfied as WM in segmentation tool, no refinement, return 1.
  }
}


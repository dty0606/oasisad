neighbor_cal <- function(wm_pve,
                         nei_seg,
                         nei_wm,
                         label,
                         re_value){

  # if not surrounded by all white matter, return mean of wmpve
  if(wm_pve == 1){
    if(sum(nei_seg == label) < length(nei_seg)){
      return(mean(nei_wm))
    } else {
      # else return a large value (>1) to suppress its probability as WMH
      if(is.null(re_value)){
        return(10)
      } else {
        return(re_value)
      }
    }
  } else {
    # if the voxel is not classfied as WM in segmentation tool, no refinement, return 1.
    return(1)
  }
}

#' @title OASISAD refine probability function
#' @description Refine voxel's probability of being
#' white matter hypeintensity by neighbor information. Neighbor
#' @param x A dataframe includes a voxel's probability of being white matter
#' and its nearest neighor voxels' vetorized indice.
#' @param neighor A boolean whether using neighor function to correct probability map
#' @param seg File path of segmentation mask segmentation tool such as FSL
#' @param wm File path of white matter pve from segmentation tool such as FSL
#' @param wm_label A value indicates white matter label in input seg_voxel argument.
#' If null, it is 3, by FSL.
#' @param re_value A value used in neighbor calculation function to
#' suppress probability of being WMH
#' @param gs A boolean whether using Gaussian Smoothing to correct probability map
#' @return Refined probability map

oasis_refine <- function(prob_map,
                         brain_mask,
                         neighbor = FALSE,
                         seg = NULL,
                         wm = NULL,
                         wm_label = NULL,
                         re_value = NULL,
                         indx = NULL
                         ){

    if(neighbor){
      # indice of voxels
      indx <- which(brain_mask == 1)

      # nearest neighbors indices
      indx_nei <- neighbor_indx(indx, dim(brain_mask))

      # find neighbor in brain mask
      indx_nei_brain <- sapply(1:6,function(i) indx_nei %in% which(brain_mask == 1))

      #calculate neighbor function value for each voxel
      if(is.null(wm_label)){
        label <- 3
      } else {
        label <- wm_label
      }
      nei_cal <- sapply_pb(1:length(prob_map), function(i){
        # indice to use
        indice <- indx_nei[i,]
        indice <- indice[which(indx_nei_brain[i,])]
        neighbor_cal(nei_seg = seg[indice],
                     wm_pve = wm[indice],
                     label = label,
                     re_value = re_value)
      })

      #refine probability map with neighbor information
      prob_map <- prob_map^nei_cal
    }


    #Gaussian smoothing
    prob_mask <- brain_mask
    prob_mask[prob_mask != 0] <- prob_map
    prob_map <- fslsmooth(prob_mask, sigma = 1.25,
                          mask = brain_mask, retimg = TRUE,
                          smooth_mask = TRUE)

    #return refined probability
    return(prob_map[prob_map != 0])
  }








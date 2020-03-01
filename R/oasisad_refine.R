#' @title OASISAD refine probability function
#' @description Refine voxel's probability of being
#' white matter hypeintensity by neighbor information. Neighbor
#' @param prob_map A dataframe includes a voxel's probability of being white matter
#' and its nearest neighor voxels' vetorized indice.
#' @param neighbor A boolean whether using neighor function to correct probability map
#' @param seg File path of segmentation mask segmentation tool such as FSL
#' Please set non-brain tissue in seg to 0
#' @param wm File path of white matter pve from segmentation tool such as FSL
#' @param wm_label A value indicates white matter label in input seg_voxel argument.
#' If null, it is 3, by FSL.
#' @param re_value A value used in neighbor calculation function to
#' suppress probability of being WMH
#' @param indx Voxels' vectorized indices
#' @param indx3d Voxels' 3D indices
#' @return Refined probability map
#' @export
#' @importFrom  neurobase niftiarr
#' @importFrom fslr fsl_smooth

oasis_refine <- function(prob_map,
                         neighbor = FALSE,
                         seg = NULL,
                         wm = NULL,
                         wm_label = NULL,
                         re_value = NULL,
                         indx = NULL,
                         indx3d = NULL
                         ){

    if(neighbor){
      # nearest neighbors indices
      indx_nei <- sapply_pb(1:nrow(indx3d), function(i)
                                            neighbor_indx(unlist(indx3d[i,]), dim(seg)))

      # find neighbor in brain mask, seg == 0 is non-brain tissue in FSL
      indx_nei_brain <- sapply(1:6,function(i) indx_nei[i,] %in% which(seg != 0))

      #calculate neighbor function value for each voxel
      if(is.null(wm_label)){
        label <- 3
      } else {
        label <- wm_label
      }

      nei_cal <- sapply_pb(1:length(prob_map), function(i){
        # indice to use
        if(sum(indx_nei_brain) == 0){
          return(1) #if no neighbor, no refinement
        } else {
          indice <- indx_nei[,i]
          indice <- indice[which(indx_nei_brain[i,])]
          return(neighbor_cal(wm_pve = wm[indx[i]],
                              nei_seg = seg[indice],
                              nei_wm = wm[indice],
                              label = label,
                              re_value = re_value))
        }

      })
      #refine probability map with neighbor information
      prob_map <- prob_map^nei_cal
    }

    #Gaussian smoothing
    prob_mask <- niftiarr(seg, 0)
    prob_mask[indx] <- prob_map
    brain_mask <- seg
    brain_mask[which(seg != 0 )] = 1
    prob_map <- fslsmooth(prob_mask, sigma = 1.25,
                          mask = brain_mask, retimg = TRUE,
                          smooth_mask = TRUE)

    #return refined probability
    return(prob_map[indx])
  }








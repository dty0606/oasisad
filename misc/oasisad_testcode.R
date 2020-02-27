#data
setwd("data")
library(neurobase)
library(extrantsr)
library(ANTsR)
library(fslr)
library(parallel)

#ids
train_id <- c('0097RS','0120LB')
valid_id <- '0177CR'
test_id <- '0064KW'

#read data
train_raw <- list()
train_raw$flair <- list(readnii('0097RS_flair.nii'),
                     readnii('0120LB_flair.nii'))
train_raw$t1 <- list(readnii('0097RS_hires+acpc.nii.gz'),
                             readnii('0120LB_hires+acpc.nii'))
train_raw$gs <- list(readnii('0097RS_flair_tracing.nii.gz'),
                     readnii('0120LB_flair_tracing.nii'))

valid_raw <- list()
valid_raw$flair <- readnii('0177CR_flair.nii')
valid_raw$t1 <- readnii('0177CR_hires+acpc.nii.gz')
valid_raw$gs <- readnii('0177CR_flair_tracing.nii')

test_raw <- list()
test_raw$flair <- readnii('0064KW_flair.nii')
test_raw$t1 <- readnii('0064KW_hires+acpc.nii')
test_raw$gs <- readnii('0064KW_flair_tracing.nii.gz')

#oasis dataframe
flair = train_raw$flair[[1]] ##flair volume of class nifti
t1 = train_raw$t1[[1]]  ##t1 volume of class nifti
t2 = NULL ##t2 volume of class nifti
pd = NULL ##pd volume of class nifti
gold_standard = train_raw$gs[[1]] ##gold standard mask of class nifti
preproc = TRUE ##option to preprocess the data
img_space = NULL
brain_mask = NULL ##brain mask of class nifti
voxel_select = NULL ##a specifed level to remove voxels whose intensity under
normalize = TRUE ##option to normalize
image_sm = TRUE ## option to smooth image
slices = NULL #slice vector
orientation = c("axial", "coronal", "sagittal")
return_preproc = FALSE
cores = 1
verbose = TRUE
eroder = TRUE
dir = 'fslout/train1'

# training sample dataframe list
train_list <- list()
for(i in 1:length(train_id)){
  train_list[[i]] <- oasisad_df(flair = train_raw$flair[[i]], ##flair volume of class nifti
                                 t1 = train_raw$t1[[i]], ##t1 volume of class nifti
                                 t2 = NULL, ##t2 volume of class nifti
                                 pd = NULL, ##pd volume of class nifti
                                 gold_standard = train_raw$gs[[i]], ##gold standard mask of class nifti
                                 preproc = TRUE, ##option to preprocess the data
                                 segmentation = TRUE,
                                 brain_mask = NULL, ##brain mask of class nifti
                                 img_space = NULL, ## if create brain mask, use T1 or FLAIR
                                 voxel_select = NULL, ##a specifed level to remove voxels whose intensity under
                                 normalize = TRUE, ##option to normalize
                                 image_sm = TRUE, ## option to smooth image
                                 slices = NULL, #slice vector
                                 orientation = c("axial", "coronal", "sagittal"),
                                 return_preproc = FALSE,
                                 cores = 1,
                                 verbose = TRUE,
                                 eroder = TRUE,
                                 dir = paste0('fslout/train',i)
  )
}

# testing sample dataframe list
test_list <- list()
for(i in 1:length(test_id)){
  test_list[[i]] <- oasisad_df(flair = test_raw$flair[[i]], ##flair volume of class nifti
                                t1 = test_raw$t1[[i]], ##t1 volume of class nifti
                                t2 = NULL, ##t2 volume of class nifti
                                pd = NULL, ##pd volume of class nifti
                                gold_standard = test_raw$gs[[i]], ##gold standard mask of class nifti
                                preproc = TRUE, ##option to preprocess the data
                                segmentation = TRUE,
                                neighbor = TRUE,
                                brain_mask = NULL, ##brain mask of class nifti
                                img_space = NULL, ## if create brain mask, use T1 or FLAIR
                                voxel_select = NULL, ##a specifed level to remove voxels whose intensity under
                                normalize = TRUE, ##option to normalize
                                image_sm = TRUE, ## option to smooth image
                                slices = NULL, #slice vector
                                orientation = c("axial", "coronal", "sagittal"),
                                return_preproc = FALSE,
                                cores = 1,
                                verbose = TRUE,
                                eroder = TRUE,
                                dir = paste0('fslout/test',i)
  )
}




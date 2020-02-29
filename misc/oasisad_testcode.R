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
valid_raw$flair <- list(readnii('0177CR_flair.nii'))
valid_raw$t1 <- list(readnii('0177CR_hires+acpc.nii.gz'))
valid_raw$gs <- list(readnii('0177CR_flair_tracing.nii'))

test_raw <- list()
test_raw$flair <- list(readnii('0064KW_flair.nii'))
test_raw$t1 <- list(readnii('0064KW_hires+acpc.nii'))
test_raw$gs <- list(('0064KW_flair_tracing.nii.gz'))

#oasis dataframe
flair = train_raw$flair[[1]] ##flair volume of class nifti
t1 = train_raw$t1[[1]]  ##t1 volume of class nifti
t2 = NULL ##t2 volume of class nifti
pd = NULL ##pd volume of class nifti
gold_standard = train_raw$gs[[1]] ##gold standard mask of class nifti
preproc = TRUE ##option to preprocess the data
brain_mask = NULL ##brain mask of class nifti
img_space = NULL
neighbor = TRUE
wm_mask = NULL
seg_mask = NULL
dir = 'fslout/train1'
eroder = TRUE
voxel_select = NULL ##a specifed level to remove voxels whose intensity under
normalize = TRUE ##option to normalize
image_sm = TRUE ## option to smooth image
slices = NULL #slice vector
orientation = c("axial", "coronal", "sagittal")
return_preproc = FALSE
cores = 1
verbose = TRUE

# training sample dataframe list
train_list <- list()
for(i in 1:length(train_id)){
  train_list[[i]] <- oasisad_df(flair = train_raw$flair[[i]], ##flair volume of class nifti
                                 t1 = train_raw$t1[[i]], ##t1 volume of class nifti
                                 t2 = NULL, ##t2 volume of class nifti
                                 pd = NULL, ##pd volume of class nifti
                                 gold_standard = train_raw$gs[[i]], ##gold standard mask of class nifti
                                 preproc = TRUE, ##option to preprocess the data
                                 brain_mask = NULL, ##brain mask of class nifti
                                 img_space = NULL, ## if create brain mask, use T1 or FLAIR
                                 neighbor = TRUE,
                                 wm_mask = NULL,
                                 seg_mask = NULL,
                                 dir = paste0('fslout/train',i),
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
}
save(train_list, file = file.path('df/train_df.rda'))

# testing sample dataframe list
test_list <- list()
for(i in 1:length(test_id)){
  test_list[[i]] <- oasisad_df(flair = test_raw$flair[[i]], ##flair volume of class nifti
                               t1 = test_raw$t1[[i]], ##t1 volume of class nifti
                               t2 = NULL, ##t2 volume of class nifti
                               pd = NULL, ##pd volume of class nifti
                               gold_standard = test_raw$gs[[i]], ##gold standard mask of class nifti
                               preproc = TRUE, ##option to preprocess the data
                               brain_mask = NULL, ##brain mask of class nifti
                               img_space = NULL, ## if create brain mask, use T1 or FLAIR
                               neighbor = TRUE,
                               wm_mask = NULL,
                               seg_mask = NULL,
                               dir = paste0('fslout/test',i),
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
}
save(test_list, file = file.path('df/test_df.rda'))

# validation sample dataframe list. Use of optimal threshold later.
valid_list <- list()
for(i in 1:length(valid_id)){
  valid_list[[i]] <- oasisad_df(flair = valid_raw$flair[[i]], ##flair volume of class nifti
                               t1 = valid_raw$t1[[i]], ##t1 volume of class nifti
                               t2 = NULL, ##t2 volume of class nifti
                               pd = NULL, ##pd volume of class nifti
                               gold_standard = valid_raw$gs[[i]], ##gold standard mask of class nifti
                               preproc = TRUE, ##option to preprocess the data
                               brain_mask = NULL, ##brain mask of class nifti
                               img_space = NULL, ## if create brain mask, use T1 or FLAIR
                               neighbor = TRUE,
                               wm_mask = NULL,
                               seg_mask = NULL,
                               dir = paste0('fslout/valid',i),
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
}
save(valid_list, file = file.path('df/valid_df.rda'))

# load df
load('df/train_df.rda')
load('df/valid_df.rda')
load('df/test_df.rda')

train_df = train_list
test_df = test_list
valid_df = valid_list
threshold = NULL
M1 = TRUE
refine = TRUE
neighbor = TRUE
wm_label = NULL
re_value = NULL

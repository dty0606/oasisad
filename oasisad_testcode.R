#data
library(neurobase)
library(devtools)
library(extrantsr)
install_github("dty0606/oasisad")
library(oasisad)

#load data
setwd('../data/')
train_raw <-list(flair = list(), t1 = list(), gs = list())
valid_raw <-list(flair = list(), t1 = list(), gs = list())
test_raw <-list(flair = list(), t1 = list(), gs = list())

train_raw$flair[[1]] <- readnii('train1_flair.nii.gz')
train_raw$t1[[1]] <- readnii('train1_t1.nii.gz')
train_raw$gs[[1]] <- readnii('train1_gs.nii.gz')

valid_raw$flair[[1]] <- readnii('valid1_flair.nii.gz')
valid_raw$t1[[1]] <- readnii('valid1_t1.nii.gz')
valid_raw$gs[[1]] <- readnii('valid1_gs.nii.gz')

test_raw$flair[[1]] <- readnii('test1_flair.nii.gz')
test_raw$t1[[1]] <- readnii('test1_t1.nii.gz')
test_raw$gs[[1]] <- readnii('test1_gs.nii.gz')


#####################
#OASISAD support N3 bias correction using fslr.
#If you need N4 bias correction, please use fslbet_robust function in extrantsr package,
#and input to brain_mask argument
#####################


# training sample dataframe list
train_list <- list()
for(i in 1:length(train_raw)){
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
                                 return_pre = FALSE,
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
                               return_pre = FALSE,
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
                               return_pre = FALSE,
                               cores = 1,
                               verbose = TRUE
  )
}
save(valid_list, file = file.path('df/valid_df.rda'))

# load df
load('df/train_df.rda')
load('df/valid_df.rda')
load('df/test_df.rda')

model_res <- oasisad_model(train_df = train_df,
                           test_df = test_df,
                           valid_df = valid_df,
                           M1 = TRUE,
                           refine = TRUE,
                           neighbor = TRUE,
                           wm_label = NULL,
                           re_value = NULL)

#evaluate
preds <- model_res$probs[[1]] > model_res$cutoff
eva <- oasisad_eva(pred = preds,
                   truth = test_df[[1]]$data$GoldStandard)
eva

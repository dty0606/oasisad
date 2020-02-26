#data
setwd()
library(neurobase)

dir_img <- 'C:/Users/dty/Desktop/oasis/Dana_image/all_20_subjects'

train_id <- c('0097RS','0120LB')
valid_id <- '0177CR'
test_id <- '0064KW'

#read data
train_raw <- list()
train_raw$flair <- lapply(train_id, function(i)
                   readnii(file.path(dir_img,'FLAIR',paste0(i,'_flair.nii'))))

train_raw$t1 <- list(readnii(file.path(dir_img,'T1w','0097RS_hires+acpc.nii.gz')),
                             readnii(file.path(dir_img,'T1w','0120LB_hires+acpc.nii')))
train_raw$gs <- list(readnii(file.path(dir_img,'manual_tracing','0097RS_flair_tracing.nii.gz')),
                     readnii(file.path(dir_img,'manual_tracing','0120LB_flair_tracing.nii')))

valid_raw <- list()
valid_raw$flair <- readnii(file.path(dir_img,'FLAIR','0177CR_flair.nii'))
valid_raw$t1 <- readnii(file.path(dir_img,'T1w','0177CR_hires+acpc.nii.gz'))
valid_raw$gs <- readnii(file.path(dir_img,'manual_tracing','0177CR_flair_tracing.nii'))

valid_raw <- list()
valid_raw$flair <- readnii(file.path(dir_img,'FLAIR','test','0064KW_flair.nii'))
valid_raw$t1 <- readnii(file.path(dir_img,'T1w','0064KW_hires+acpc.nii.gz'))
valid_raw$gs <- readnii(file.path(dir_img,'manual_tracing','0064KW_flair_tracing.nii'))


#oasis dataframe
flair = train_raw$flair[[1]] ##flair volume of class nifti
t1 = train_raw$t1[[1]]  ##t1 volume of class nifti
t2 = NULL ##t2 volume of class nifti
pd = NULL ##pd volume of class nifti
gold_standard = train_raw$gs[[1]] ##gold standard mask of class nifti
brain_mask = NULL ##brain mask of class nifti
voxel_select = NULL ##a specifed level to remove voxels whose intensity under
preproc = TRUE ##option to preprocess the data
normalize = TRUE ##option to normalize
image_sm = TRUE ## option to smooth image
slices = NULL #slice vector
orientation = c("axial", "coronal", "sagittal")
return_preproc = FALSE
cores = 1
verbose = TRUE
eroder = TRUE
dir = 'C:/Users/dty/Desktop/test_oasisad'





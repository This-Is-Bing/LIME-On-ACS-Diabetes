# ---------------------------------------------------------------------------#
####                        File Description & Setup                      ####
#----------------------------------------------------------------------------#
# Please run the rproj file in the folder                                     
# Dataset: Diabetes ACS, STEMI, NSTEMI on Testing Dataset                                    
# Description: To compare the NRI Performance

##### Cleaning environment ####
rm(list=ls())

##### Loading Library ####
set.seed(2468)
source('./00_Functions.R')
library(lime)
library(e1071)
library(readxl)
library(caret)
library(eRic)
library(ResourceSelection)
library(rms)
library(pROC)


# ---------------------------------------------------------------------------#
####                       Loading and Transformation                     ####
#----------------------------------------------------------------------------#

##### Reading Testing Dataset
training_ds = read.csv("./data/processed/training_ds.csv", header = TRUE)
testing_ds <- read.csv('./data/processed/testing_ds.csv', header = TRUE)

##### Get all Features ####
all_features <- read_xlsx('./data/NCVD_features_dm.xlsx')

##### Structuring each feature ####
training_ds <- RF20_TransformationFunction(training_ds, all_features)
testing_ds <- RF20_TransformationFunction(testing_ds, all_features)

##### Separating ACS, STEMI, NSTEMI
stemi_testing_ds <- testing_ds[testing_ds$acsstratum ==1, ]
nstemi_testing_ds <- testing_ds[!testing_ds$acsstratum ==1, ]

##### Removing acsstratum, timiscorestemi, timiscorenstemi ####
cols_to_remove <- c('acsstratum', 'timiscorestemi', 'timiscorenstemi')
testing_ds <- testing_ds[, -which(names(testing_ds) %in% cols_to_remove)]
stemi_testing_ds <- stemi_testing_ds[, -which(names(stemi_testing_ds) %in% cols_to_remove)]
nstemi_testing_ds <- nstemi_testing_ds[, -which(names(nstemi_testing_ds) %in% cols_to_remove)]

##### Separating features and label ####
X_training_ds <- training_ds[,-which(names(training_ds) == 'ptoutcome')]
X_testing_ds <- testing_ds[,-which(names(testing_ds) == 'ptoutcome')]
X_stemi_testing_ds <- stemi_testing_ds[,-which(names(stemi_testing_ds) == 'ptoutcome')]
X_nstemi_testing_ds <- nstemi_testing_ds[,-which(names(nstemi_testing_ds) == 'ptoutcome')]

y_training_ds <- training_ds[,which(names(training_ds) == 'ptoutcome')]
y_testing_ds <- testing_ds[,which(names(testing_ds) == 'ptoutcome')]
y_stemi_testing_ds <- stemi_testing_ds[,which(names(stemi_testing_ds) == 'ptoutcome')]
y_nstemi_testing_ds <- nstemi_testing_ds[,which(names(nstemi_testing_ds) == 'ptoutcome')]

# ---------------------------------------------------------------------------#
####                        Predicting Probabilities                      ####
#----------------------------------------------------------------------------#

##### Loading Models ####
raw_model <- readRDS('./models/3_modelAll_rf20_dm.rds')$RF
acs_calibrated_model <- readRDS('./models/acs_calibrated_model.rds')
stemi_calibrated_model <- readRDS('./models/stemi_calibrated_model.rds')
nstemi_calibrated_model <- readRDS('./models/nstemi_calibrated_model.rds')

##### Prediction ####

###### ACS ####
acs_raw_pred_probs <- predict(raw_model, X_testing_ds, type= 'prob')$Death
acs_calibrated_pred_probs <- predict(acs_calibrated_model, data.frame(y = y_testing_ds, x = acs_raw_pred_probs), type = 'response')

###### STEMI ####
stemi_raw_pred_probs <- predict(raw_model, X_stemi_testing_ds, type= 'prob')$Death
stemi_calibrated_pred_probs <- predict(stemi_calibrated_model, data.frame(y = y_stemi_testing_ds, x = stemi_raw_pred_probs), type = 'response')

###### NSTEMI ####
nstemi_raw_pred_probs <- predict(raw_model, X_nstemi_testing_ds, type= 'prob')$Death
nstemi_calibrated_pred_probs <- predict(nstemi_calibrated_model, data.frame(y = y_nstemi_testing_ds, x = nstemi_raw_pred_probs), type = 'response')

###### ACS—STEMI ####
acs_stemi_raw_pred_probs <- predict(raw_model, X_stemi_testing_ds, type= 'prob')$Death

###### ACS-NSTEMI ####
acs_nstemi_raw_pred_probs <- predict(raw_model, X_nstemi_testing_ds, type= 'prob')$Death

# ---------------------------------------------------------------------------#
####                                  NRI                                 ####
#----------------------------------------------------------------------------# 
library(nricens)

##### Search Threshold for Classification ####
raw_threshold<-SearchBestThreshold(y_testing_ds,acs_raw_pred_probs)
calibrated_threshold<-SearchBestThreshold(y_testing_ds,acs_calibrated_pred_probs)

stemi_raw_threshold<-SearchBestThreshold(y_stemi_testing_ds,stemi_raw_pred_probs)
calibrated_stemi_raw_threshold<-SearchBestThreshold(y_stemi_testing_ds,stemi_calibrated_pred_probs)

nstemi_raw_threshold<-SearchBestThreshold(y_nstemi_testing_ds,nstemi_raw_pred_probs)
calibrated_nstemi_raw_threshold<-SearchBestThreshold(y_nstemi_testing_ds,nstemi_calibrated_pred_probs)

stemi_acs_raw_threshold<-SearchBestThreshold(y_stemi_testing_ds,acs_stemi_raw_pred_probs)

nstemi_acs_raw_threshold<-SearchBestThreshold(y_nstemi_testing_ds,nstemi_raw_pred_probs)


##### Changing into Class 1,0 ####
acs_raw_pred_probs_class <- ifelse(acs_raw_pred_probs>=raw_threshold, 1,0)
acs_calibrated_pred_probs_class <- ifelse(acs_calibrated_pred_probs>=calibrated_threshold, 1,0)

stemi_raw_pred_probs_class <- ifelse(stemi_raw_pred_probs>=stemi_raw_threshold, 1,0)
stemi_calibrated_pred_probs_class <- ifelse(stemi_calibrated_pred_probs>=calibrated_stemi_raw_threshold, 1,0)

nstemi_raw_pred_probs_class <- ifelse(nstemi_raw_pred_probs>=nstemi_raw_threshold, 1,0)
nstemi_calibrated_pred_probs_class <- ifelse(nstemi_calibrated_pred_probs>=calibrated_nstemi_raw_threshold, 1,0)

stemi_acs_raw_pred_probs_class <- ifelse(acs_stemi_raw_pred_probs>=stemi_acs_raw_threshold, 1,0)

nstemi_acs_raw_pred_probs_class <- ifelse(acs_nstemi_raw_pred_probs>=nstemi_acs_raw_threshold, 1,0)

##### Getting TIMI Score ####
testing_ds <- read.csv('./data/processed/testing_ds.csv', header = TRUE)
testing_ds$timiscorenstemi <- ifelse(testing_ds$timiscorenstemi >=5,1,0)
testing_ds$timiscorestemi <- ifelse(testing_ds$timiscorestemi >=5,1,0)

##### Separating  STEMI, NSTEMI ####
stemi_testing_ds <- testing_ds[testing_ds$acsstratum ==1, ]
nstemi_testing_ds <- testing_ds[!testing_ds$acsstratum ==1, ]

##### Removing column ####
stemi_testing_ds <- stemi_testing_ds[, -which(names(stemi_testing_ds) %in% c('acsstratum', 'timiscorenstemi'))]
nstemi_testing_ds <- nstemi_testing_ds[, -which(names(nstemi_testing_ds) %in% c('acsstratum','timiscorestemi'))]

##### TIMI Score ####
testing_ds$timiscore <- ifelse(is.na(testing_ds$timiscorestemi), testing_ds$timiscorenstemi,testing_ds$timiscorestemi )
timiscore <- testing_ds$timiscore

acs_nri = nribin(event = y_testing_ds, 
                 p.std=timiscore, 
                 p.new=acs_calibrated_pred_probs_class, 
                 cut = 0.5,
                 niter = 5000, 
                 updown = 'category')

stemi_nri = nribin(event = stemi_testing_ds$ptoutcome, 
                   p.std=stemi_testing_ds$timiscorestemi, 
                   p.new=stemi_calibrated_pred_probs_class, 
                   cut = 0.5,
                   niter = 5000, 
                   updown = 'category')

nstemi_nri = nribin(event = nstemi_testing_ds$ptoutcome, 
                    p.std=nstemi_testing_ds$timiscorenstemi, 
                    p.new=nstemi_calibrated_pred_probs_class, 
                    cut = 0.5,
                    niter = 5000, 
                    updown = 'category')



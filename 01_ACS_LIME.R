# ---------------------------------------------------------------------------#
####                        File Description & Setup                      ####
#----------------------------------------------------------------------------#
# Please run the rproj file in the folder                                     
# Dataset: Diabetes ACS, STEMI, NSTEMI on Testing Dataset                                    
# Description: To compare the raw model & calibrated model performance 


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
testing_ds_noNorm <- RF20_TransformationFunction_noNorm(testing_ds, all_features)
testing_ds <- RF20_TransformationFunction(testing_ds, all_features)

##### Separating ACS, STEMI, NSTEMI
stemi_testing_ds <- testing_ds[testing_ds$acsstratum ==1, ]
nstemi_testing_ds <- testing_ds[!testing_ds$acsstratum ==1, ]

##### Removing acsstratum, timiscorestemi, timiscorenstemi ####
cols_to_remove <- c('acsstratum', 'timiscorestemi', 'timiscorenstemi')
testing_ds <- testing_ds[, -which(names(testing_ds) %in% cols_to_remove)]
testing_ds_noNorm <- testing_ds_noNorm[, -which(names(testing_ds_noNorm) %in% cols_to_remove)]
stemi_testing_ds <- stemi_testing_ds[, -which(names(stemi_testing_ds) %in% cols_to_remove)]
nstemi_testing_ds <- nstemi_testing_ds[, -which(names(nstemi_testing_ds) %in% cols_to_remove)]

##### Separating features and label ####
X_training_ds <- training_ds[,-which(names(training_ds) == 'ptoutcome')]
X_testing_ds <- testing_ds[,-which(names(testing_ds) == 'ptoutcome')]
X_testing_ds_noNorm <- testing_ds_noNorm[,-which(names(testing_ds_noNorm) == 'ptoutcome')]
X_stemi_testing_ds <- stemi_testing_ds[,-which(names(stemi_testing_ds) == 'ptoutcome')]
X_nstemi_testing_ds <- nstemi_testing_ds[,-which(names(nstemi_testing_ds) == 'ptoutcome')]

y_training_ds <- training_ds[,which(names(training_ds) == 'ptoutcome')]
y_testing_ds <- testing_ds[,which(names(testing_ds) == 'ptoutcome')]
y_testing_ds_noNorm <- testing_ds_noNorm[,which(names(testing_ds_noNorm) == 'ptoutcome')]
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


# ---------------------------------------------------------------------------#
####                                 LIME                                 ####
#----------------------------------------------------------------------------# 

##### Reading Training dataset #####
explainer <- lime(X_training_ds, raw_model)
y_testing_ds[117]
explanation <- explain(X_testing_ds[1:5, ], explainer, n_labels = 1, n_features = 10, n_permutations = 3, set.seed(10))
plot_features(explanation)

death_cases <- predict(raw_model, X_testing_ds, type= 'raw')
death_index <- death_cases == 'Death'
explanation <- explain(head(X_testing_ds[death_index,],4), explainer, n_labels = 1, n_features = 20, n_permutations = 30, set.seed(10))
plot_features(explanation)

##### no Normalization ####
explainer <- lime(X_training_ds, raw_model)
explanation <- explain(X_testing_ds_noNorm[117, ], explainer, n_labels = 1, n_features = 20, n_permutations = 30, set.seed(10))
plot_features(explanation)

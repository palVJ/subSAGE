#Paired bootstrapping og modified SAGE.

library(xgboost)
library(Rcpp)
library(data.table)

args <- commandArgs(trailingOnly = TRUE)
arID = as.numeric(args[1])
xgbmodelfile = as.character(args[2])
datafile = as.character(args[3])
feature = as.character(args[4])

#Load xgboost model:
load(file = xgbmodelfile)
model = xgb.Booster.complete(model)

#Load data to do bootstrapping on:
load(file = datafile) 
data = as.data.table(DataForTraining)


#Resample data:
smpl = sample(1:nrow(data),nrow(data),replace = TRUE)
resampled_data = data[smpl,]

source("~/SHAPInference/SAGE/ModSAGE.R")
t = xgb.model.dt.tree(model = model)
trees = as.data.table(xgboost.trees(xgb_model = model, data = resampled_data, recalculate = FALSE))
B = ModSage_cpp(resampled_data,trees,feature)
save(B,file = paste("~/SHAPInference/SAGE/Boostraps/BootTrain",arID,"feat",feature,".RData",sep = ""))

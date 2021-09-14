#Paired bootstrapping of subSAGE, one iteration.

library(xgboost)
library(Rcpp)
library(data.table)

args <- commandArgs(trailingOnly = TRUE)
arID = as.numeric(args[1])
xgbmodelfile = as.character(args[2])
testdatafile = as.character(args[3])
feature = as.character(args[4])

#Load xgboost model:
load(file = xgbmodelfile)
model = xgb.Booster.complete(model)

#Load data to do bootstrapping on:
data = fread(file = testdatafile,header = TRUE,sep = " ")


#Resample data:
smpl = sample(1:nrow(data),nrow(data),replace = TRUE)
resampled_data = data[smpl,]

source("~/subSAGE/subSAGE.R")
t = xgb.model.dt.tree(model = model)
trees = as.data.table(xgboost.trees(xgb_model = model, data = resampled_data, recalculate = FALSE))
B = subSage_cpp(resampled_data,trees,feature)
save(B,file = paste("~/subSAGE/Bootstraps/Boot",arID,"feat",feature,".RData",sep = ""))

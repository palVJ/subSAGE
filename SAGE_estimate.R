library(xgboost)
library(Rcpp)
library(data.table)

args <- commandArgs(trailingOnly = TRUE)
xgbmodelfile = as.character(args[1])
datafile = as.character(args[2])
feature = as.character(args[3])

#Load xgboost model:
load(file = xgbmodelfile)
model = xgb.Booster.complete(model)

#Load data to do estimation:
load(file = datafile)
data = as.data.table(DataForTraining)

source("~/project/SAGE/ModSAGE.R")
t = xgb.model.dt.tree(model = model)
trees = as.data.table(xgboost.trees(xgb_model = model, data = data, recalculate = FALSE))
estimate = ModSage_cpp(data,trees,feature,loss = "RMSE")
save(estimate,file = paste("~/project/SAGE/SAGE_estimateOnTrain","feat",feature,".RData",sep = ""))


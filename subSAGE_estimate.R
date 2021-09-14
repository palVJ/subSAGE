library(xgboost)
library(Rcpp)
library(data.table)

args <- commandArgs(trailingOnly = TRUE)
xgbmodelfile = as.character(args[1])
testdatafile = as.character(args[2])
feature = as.character(args[3])

#Load xgboost model:
load(file = xgbmodelfile)
model = xgb.Booster.complete(model)

#Load data to do estimation:
data = fread(file = testdatafile,header = TRUE,sep = " ")

source("~/subSAGE/subSAGE.R")
t = xgb.model.dt.tree(model = model)
trees = as.data.table(xgboost.trees(xgb_model = model, data = data, recalculate = FALSE))
estimate = subSage_cpp(data,trees,feature,loss = "RMSE")
save(estimate,file = paste("~/subSAGE/subSAGE_estimate","feat",feature,".RData",sep = ""))


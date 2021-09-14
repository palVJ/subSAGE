library(xgboost)
load("~/SHAPInference/SAGE/xgbmodel_V2.RData")
model = xgb.Booster.complete(model)

load("DataForTraining_sdnoise2_V2.RData")
DataForTraining = as.matrix(DataForTraining[,1:100])
#Compute SHAP value using training data:

shaps = predict(object = model, newdata = DataForTraining,predcontrib = TRUE)


#Compute ERFC for each feature:

ERFCs = rep(0,ncol(shaps)-1)
names(ERFCs) = colnames(shaps)[1:(ncol(shaps)-1)]

denoms = apply(shaps,1,function(x) sum(abs(x)))

for(i in 1:length(ERFCs)){


ERFCs[i] = mean(abs(shaps[,i])/denoms)


}

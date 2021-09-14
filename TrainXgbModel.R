#return xgboost model trained on known function
#Create data and compute the function

TrainXgbModel = function(file){

#Load training data:
load(file)

#Split in train (50%), validation (30 %) and test (20%):
Ntrains = nrow(DataForTraining)
train = DataForTraining[1:(round(Ntrains*(5/8))),-ncol(DataForTraining)]
val = DataForTraining[(round(Ntrains*(5/8))+1):Ntrains,-ncol(DataForTraining)]
trainresponse = DataForTraining[1:(round(Ntrains*(5/8))),ncol(DataForTraining)]
valresponse = DataForTraining[(round(Ntrains*(5/8))+1):Ntrains,ncol(DataForTraining)]


#load xgboost package
library(xgboost)

TrainAsMatrix = data.matrix(train)
dtrain = xgb.DMatrix(data=TrainAsMatrix,label = trainresponse)
ValAsMatrix = data.matrix(val)
dval = xgb.DMatrix(data=ValAsMatrix,label = valresponse)

#train model
model <- xgb.train(data = dtrain, max_depth = 2,
                   watchlist <- list(eval=dval),param = list(eta = 0.05,subsample = 0.7,lambda = 1,gamma = 0,colsample_bytree = 0.8), nthread = 2, nrounds = 5000, objective = "reg:squarederror",
                   early_stopping_rounds = 20)

#save model
save(model,file = "xgbmodel_V2.RData")

}

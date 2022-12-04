library(xgboost)
load("~/subSAGE/R/Figures/xgbmodel.RData")
library(scales)


load("~/subSAGE/R/Figures/DataForTraining_sdnoise2.RData")
DataForTraining = DataForTraining[,1:100]
#Reduce set:
DataForTraining = DataForTraining[sample(1:nrow(DataForTraining),1000,replace = FALSE),]
#Compute SHAP value using training data:

shaps = predict(object = model, newdata = as.matrix(DataForTraining),predcontrib = TRUE)


x6 = DataForTraining$x6
x5 = DataForTraining$x5
x1 = DataForTraining$x1
x2 = DataForTraining$x2
x12 = DataForTraining$x12

Ex1 = 0.8
Ex2 = 0.08
Ex5 = 15
Ex6lt7 = 0.242
Eexpx2 = 1.142


#Exact relationship between x-value and corresponding shap-value:
shapx6 = -Ex5*((x6 > 7)-Ex6lt7) -0.5*(x6 > 7)*(x5-Ex5)+0.5*Ex6lt7*(x5-Ex5)
shapx1 = 0.03*(x1-Ex1) + 0.3*Eexpx2*(x1-Ex1) + 0.15*x1*(exp(x2)-Eexpx2)-0.15*Ex1*(exp(x2)-Eexpx2)
shapx2 = -0.05*(x2-Ex2) + 0.3*Ex1*(exp(x2)-Eexpx2) + 0.15*x1*(exp(x2)-Eexpx2)-0.15*Ex1*(exp(x2)-Eexpx2)

dfx6 = data.frame(x6,shapx6)
dfx6 = dfx6[order(dfx6[,1]),]
dfx1 = data.frame(x1,x2,shapx1)
dfx1 = dfx1[order(dfx1[,1]),]
dfx2 = data.frame(x2,x1,shapx2)
dfx2 = dfx2[order(dfx2[,1]),]

pdf("EstSHAPvsTrue.pdf")

par(mfrow = c(2,2))

plot(x6,shaps[,6],col = "orange",type = "p",ylim = c(-18,5),ylab = "SHAP")
points(dfx6[dfx6[,1] <= 7,1],dfx6[dfx6[,1] <= 7,2],col = alpha("steelblue1",0.4),lty = 2)
points(dfx6[dfx6[,1] > 7,1],dfx6[dfx6[,1] > 7,2],col = alpha("steelblue1",0.4),lty = 2)

legend("bottomleft",legend = c("Est. SHAP","True SHAP"),
	col = c("orange","steelblue1"),pch = c(1,1),cex = 0.8)

plot(x1,shaps[,1],col = "orange",ylim = c(-1.1,1.6),ylab = "")
points(dfx1[dfx1[,2] == 0,1],dfx1[dfx1[,2] == 0,3],col = "steelblue1",type = "b")
points(dfx1[dfx1[,2] == 1,1],dfx1[dfx1[,2] == 1,3],col = "steelblue3",type = "b")
points(dfx1[dfx1[,2] == 2,1],dfx1[dfx1[,2] == 2,3],col = "steelblue4",type = "b")

legend("bottomright",legend = c("Est. SHAP","True SHAP, x2 = 0","True SHAP, x2 = 1","True SHAP, x2 = 2"),
        col = c("orange","steelblue1","steelblue3","steelblue4"),pch = c(1,1,1,1),lty = c(NA,1,1,1),cex = 0.8)


plot(x2,shaps[,2],col = "orange",type = "p",ylim = c(-0.1,2.6),ylab = "SHAP")
points(dfx2[dfx2[,2] == 0,1],dfx2[dfx2[,2] == 0,3],col = "steelblue1",type = "b")
points(dfx2[dfx2[,2] == 1,1],dfx2[dfx2[,2] == 1,3],col = "steelblue3",type = "b")
points(dfx2[dfx2[,2] == 2,1],dfx2[dfx2[,2] == 2,3],col = "steelblue4",type = "b")

legend("topleft",legend = c("Est. SHAP","True SHAP, x1 = 0","True SHAP, x1 = 1","True SHAP, x1 = 2"),
        col = c("orange","steelblue1","steelblue3","steelblue4"),pch = c(1,1,1,1),lty = c(NA,1,1,1),cex = 0.8)


plot(x12,shaps[,12],col = "orange",type = "p",ylab = "")
lines(sort(x12),rep(0,length(x12)),col = "steelblue1",lty =2 )

legend("bottomleft",legend = c("Est. SHAP", "True SHAP"),col = c("orange","steelblue1"),pch = c(1,NA),lty = c(NA,1),cex = 0.8)


dev.off()

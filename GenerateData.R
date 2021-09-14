#Create data and compute the function

x1 = rbinom(n = 16000, size = 2, prob = 0.4)
x2 = rbinom(n = 16000, size = 2, prob = 0.04)
x3 = rgamma(n = 16000,shape = 10, rate = 2)
x4 = runif(n = 16000,min = 0, max = pi)
x5 = rpois(n = 16000, lambda = 15)
x6 = rnorm(n = 16000, mean = 0, sd = 10)
#add noise variables as normal distributions and binomials:
means = runif(n = 47, min = -20, max = 20)
sds = runif(n = 47, min = 1, max = 10)
probs = runif(n = 47, min = 0.02, max = 0.5)
noise = matrix(ncol = 94,nrow = 16000)
colnames(noise) = paste("x",7:100,sep = "")
for(i in 1:47){
  noise[,i] = rnorm(n = 16000, mean = means[i], sd = sds[i])
}

for(i in 48:94){
  noise[,i] = rbinom(n = 16000,size = 2, prob = probs[i-47])
}


#The unknown function depends on x1,x2,x3,x4,x5 and x6. The rest do not matter.
#Add systematic error:
error = rnorm(n = 16000,sd = 2)

data = data.frame(x1,x2,x3,x4,x5,x6,noise,error)

func = function(x){

beta_0 = -0.5
beta_1 = 0.03
beta_2 = -0.05
beta_12 = 0.3
beta_3 = 0.02
beta_4 = 0.35
beta_5 = -0.2
beta_6 = -1


response =  beta_0 + beta_1*(x$x1) + beta_2*(x$x2) + beta_12*(x$x1)/(exp(-x$x2)) +
  beta_3*(x$x3)^2 + beta_4*sin(x$x4) +  beta_5*log(1+x$x5) + beta_6*(x$x5)*(x$x6 > 7) + x$error

return(response)

}

response = func(x = data)
data = cbind(data[,-ncol(data)],response)

#Pick randomly 20 % of the data to be testdata

r = sample(1:nrow(data),round(nrow(data)*0.2),replace = FALSE)
Testdata = data[r,]

DataForTraining = data[-r,]

save(Testdata,file = "~/SHAPInference/SAGE/TestData_sdnoise2_V2.RData")
save(DataForTraining,file = "~/SHAPInference/SAGE/DataForTraining_sdnoise2_V2.RData")
save(error,file = "~/SHAPInference/SAGE/ErrorUsedInV2.RData")

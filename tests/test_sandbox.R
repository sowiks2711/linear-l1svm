library(LiblineaR)
library(l1svm)
library(dplyr)
data(iris)
data <- iris %>% filter(Species != "setosa")
x=data[,1:4]
y=factor(data[,5])
train=sample(1:dim(data)[1],100)

xTrain=x[train,]
xTest=x[-train,]
yTrain=y[train]
yTest=y[-train]

s=scale(xTrain,center=TRUE,scale=TRUE)

print(LiblineaR(data=s,target=yTrain,type=3,cost=0.1, epsilon = 0.01, bias=1, cross=0,verbose=FALSE))
l1svm(data=s,target=yTrain,cost=0.1, epsilon=0.001, bias=1)
cat("Results for C=",co," : ",acc," accuracy.\n",sep="")

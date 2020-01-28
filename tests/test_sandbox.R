library(LiblineaR)
library(l1svm)
library(dplyr)
library(SparseM)
library(microbenchmark)
library(kolejkeR)

data("warsaw_queues")
data <- warsaw_queues %>% filter(!is.na(status)) %>% select(czasObslugi, liczbaCzynnychStan, liczbaKlwKolejce, status)

data
x <- data[,1:3]
y <- data[,4]

# l1svm, l2svm, implementation

benchmark_svm <- function(size, repeats) {
  train=sample(1:dim(data)[1],size)
  xTrain=x[train,]
  yTrain=y[train]
  s=scale(xTrain,center=TRUE,scale=TRUE)

  l2svm_their <- function() LiblineaR(data=s,target=yTrain,type=1,cost=0.1, epsilon = 0.001, bias=1, cross=0,verbose=FALSE)
  l1svm_their <- function() LiblineaR(data=s,target=yTrain,type=3,cost=0.1, epsilon = 0.001, bias=1, cross=0,verbose=FALSE)
  l1svm_ours <- function() l1svm(data=s,target=yTrain,cost=0.1, epsilon=0.001, bias=1)

  microbenchmark(l2svm_their(), l1svm_their(), l1svm_ours(), times = repeats)

}

measurements2 <- lapply(seq(1000,10000,1000), benchmark_svm(repeats = 5))

measurements

data.frame(seq(1000,10000,1000), measurements)



benchmark_svm(5000, 1)
benchmark_data <- Reduce(function(x, y) rbind, measurements)

library(ggplot2)
benchmark_data %>%
  ggplot(aes(x = benchmark_data$time))

res %>% mutate(size = 100)
cbind(size=100, res)
cat("Results for C=",co," : ",acc," accuracy.\n",sep="")

# Sparsifying the iris dataset:
iS=apply(iris[,1:4],2,function(a){a[a<quantile(a,probs=c(0.25))]=0;return(a)})
irisSparse<-as.matrix.csr(iS)

# Applying a similar methodology as above:
xTrain=irisSparse[train,]
xTest=irisSparse[-train,]

# Re-train best model with best cost value.
LiblineaR(data=xTrain,target=yTrain,type=3,cost=0.1,bias=1,verbose=FALSE)

l1svm(data=xTrain,target=yTrain,cost=0.1, epsilon=0.001, bias=1)
# Make prediction
p=predict(m,xTest,proba=pr,decisionValues=TRUE)

# Display confusion matrix
res=table(p$predictions,yTest)
print(res)

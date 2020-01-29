library(LiblineaR)
library(l1svm)
library(dplyr)
library(SparseM)
library(microbenchmark)
library(kolejkeR)
library(parallel)

data("warsaw_queues")
data <- warsaw_queues %>% filter(!is.na(status)) %>% select(czasObslugi, liczbaCzynnychStan, liczbaKlwKolejce, status)

data
features <- data[,1:3]
target <- data[,4]
length(y)
# l1svm, l2svm, implementation

benchmark_svm <- function(x, y, size, repeats) {
  train=sample(1:length(y),size)
  xTrain=x[train,]
  yTrain=y[train]
  s=scale(xTrain,center=TRUE,scale=TRUE)


  l2svm_their <- function() LiblineaR(data=s,target=yTrain,type=1,cost=0.1, epsilon = 0.001, bias=1, cross=0,verbose=FALSE)
  l1svm_their <- function() LiblineaR(data=s,target=yTrain,type=3,cost=0.1, epsilon = 0.001, bias=1, cross=0,verbose=FALSE)
  l1svm_ours <- function() l1svm(data=s,target=yTrain,cost=0.1, epsilon=0.001, bias=1)

  cbind(microbenchmark(l2svm_their(), l1svm_their(), l1svm_ours(), times = repeats), size=size)

}

measurementsParallel <- mclapply(seq(1000,10000,1000), function(s) benchmark_svm(x = features, y = targets, size = s, repeats = 5), mc.cores = getOption("mc.cores", 4L))


library(ggplot2)
measurements2 %>% Reduce(rbind, .) %>% ggplot(aes(x = size, y = time, colour=expr)) +
  geom_point()

measurementsParallel %>% Reduce(rbind, .) %>% ggplot(aes(x=size, y = time, color=expr)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()



measurements[[100]]

data.frame(seq(1000,10000,1000), measurements)

library(AppliedPredictiveModeling)
data(abalone)
dim(abalone)
head(abalone)
abalone %>% group_by(Type) %>% count()
wideData <- abalone %>% mutate(Type=ifelse(Type == "F", "F", "NotF"))
wide_features <- wideData[,-1]
wide_target <- wideData[,1]

measurements_wide <- mclapply(
  seq(1000,4000,250),
  function(s) lapply(
    seq(2,8,1),
    function(sub) cbind(features=sub, benchmark_svm(x = wide_features[,1:sub], y = wide_target, size = s, repeats = 3))
  ),
  mc.cores = getOption("mc.cores", 4L)
)
measurements_wide %>% Reduce(rbind, .) %>% Reduce(rbind, .) %>%
  ggplot(aes(x = size, y = time, color = factor(features))) +
  geom_point() +
  scale_color_brewer(type="qual") +
  facet_wrap(~expr)

measurements_wide %>% Reduce(rbind, .) %>% Reduce(rbind, .) %>% write.csv(., "measurements_for_many_features.csv", col.names = TRUE)










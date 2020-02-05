library(LiblineaR)
library(l1svm)
library(dplyr)
library(SparseM)
library(microbenchmark)
library(kolejkeR)
library(parallel)
library(ggplot2)

data("warsaw_queues")
data <- warsaw_queues %>% filter(!is.na(status)) %>% select(czasObslugi, liczbaCzynnychStan, liczbaKlwKolejce, status)

data
features <- data[,1:3]
target <- data[,4]
length(y)
# l1svm, l2svm, implementation

benchmark_svm <- function(x, y, size, repeats, randomize_data = TRUE) {
  if (randomize_data) {
    train <- sample(1:length(y),size)
  } else {
    train <- 1:size
  }
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
library(stringr)
measurements2 %>% Reduce(rbind, .) %>% ggplot(aes(x = size, y = time, colour=expr)) +
  geom_point()

measurementsParallel %>% Reduce(rbind, .) %>%
  mutate(method = str_replace(str_replace(expr, "their\\(\\)", "Liblinear"), "ours\\(\\)", "ours")) %>%
  ggplot(aes(x=size, y = time, color=method)) +
  geom_point() +
  scale_y_continuous(labels = function(x) format(paste(x/10^9, "sec"))) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  ggtitle(label = "Time in relation to size of dataset for 3 features") +
  xlab("Nr of observations") +
  ylab("Time")


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

measurements_not_shuffled <- mclapply(
  seq(1000,4000,250),
  function(s) lapply(
    seq(2,8,1),
    function(sub) cbind(features=sub, benchmark_svm(x = wide_features[,1:sub], y = wide_target, size = s, repeats = 3, randomize_data = FALSE))
  ),
  mc.cores = getOption("mc.cores", 4L)
)

measurements_wide %>% Reduce(rbind, .) %>% Reduce(rbind, .) %>%
  mutate(Method = str_replace(str_replace(expr, "their\\(\\)", "Liblinear"), "ours\\(\\)", "ours"),
  `Nr of features` = factor(features),
  Observations = size,
  Time = time) %>%
  ggplot(aes(x = Observations, y = Time, color = `Nr of features`)) +
  geom_point() +
  theme_bw() +
  scale_color_brewer(type="qual") +
  facet_wrap(~Method) +
  ggtitle("Performance for different observations and nr of features", subtitle = "Every sample shuffled")


measurements_not_shuffled %>% Reduce(rbind, .) %>% Reduce(rbind, .) %>%
  mutate(Method = str_replace(str_replace(expr, "their\\(\\)", "Liblinear"), "ours\\(\\)", "ours"),
         `Nr of features` = factor(features),
         Observations = size,
         Time = time) %>%
  ggplot(aes(x = Observations, y = Time, color = `Nr of features`)) +
  scale_y_continuous(labels = function(x) format(paste(x/10^9, "sec"))) +
  geom_point() +
  scale_color_brewer(type="qual") +
  facet_wrap(~Method) +
  ggtitle("Performance for different observations and nr of features", subtitle = "Without shuffling")

measurements_dense <- mclapply(
  seq(100,1000,50),
  function(s) lapply(
    seq(2,8,1),
    function(sub) cbind(features=sub, benchmark_svm(x = wide_features[,1:sub], y = wide_target, size = s, repeats = 3, randomize_data = FALSE))
  ),
  mc.cores = getOption("mc.cores", 4L)
)

measurements_not_shuffled %>% Reduce(rbind, .) %>% Reduce(rbind, .) %>%
  mutate(Method = str_replace(str_replace(expr, "their\\(\\)", "Liblinear"), "ours\\(\\)", "ours"),
         `Nr of features` = factor(features),
         Observations = size,
         Time = time) %>%
  group_by(`Nr of features`, Method, Observations) %>%
  summarise(`Mean time` = mean(time)) %>%
  ggplot(aes(x = Observations, y = `Nr of features`, fill = `Mean time`)) +
  geom_tile() +
  facet_wrap(~Method) +
  ggtitle("Performance for different observations and nr of features", subtitle = "Without shuffling")

measurements_dense %>% Reduce(rbind, .) %>% Reduce(rbind, .) %>%
  mutate(Method = str_replace(str_replace(expr, "their\\(\\)", "Liblinear"), "ours\\(\\)", "ours"),
         `Nr of features` = factor(features),
         Observations = size,
         Time = time) %>%
  group_by(`Nr of features`, Method, Observations) %>%
  summarise(`Mean time` = mean(time)) %>%
  ggplot(aes(x = Observations, y = `Nr of features`, fill = `Mean time`)) +
  geom_tile() +
  facet_wrap(~Method) +
  ggtitle("Performance for different observations and nr of features", subtitle = "With shuffling")

measurements_wide %>% Reduce(rbind, .) %>% Reduce(rbind, .) %>% write.csv(., "measurements_for_many_features.csv", col.names = TRUE)










library(stats)
library(class)
library(gmodels)
library(tibble)
library(ggplot2)
library(caret)
library (ROCR)
library (PRROC)

##
# Shows a lot of information about knn results
# confusionMatrix(dataset_predicted, id_test_labels)
##

#Load the dataset
load("idList-cornered-100-2021.Rdata")

# Data to dataframe
dataSet <- do.call(rbind, idList[1:13]) 
dataSet <- as.data.frame(dataSet)
dataSet[,1] <- factor(dataSet[,1])

#####################
## Exercise 3.1 - K-means clustering
#####################

run_kmeans_and_knn <- function(train_data, train_data_label, test_data, test_data_labels, centers) {
  set.seed(2345)
  
  cipher_cluster <- c() 
  label_cluster <- c()
  
  for( i in 0:9) { 
    clusterData <- kmeans(train_data[ train_data_label == i, ], centers) 
    cipher_cluster[[i + 1]] <- clusterData$centers 
    label_cluster[[i + 1]] <- c(1:centers)*0 + i 
  }
  
  train_lab <- factor(unlist(label_cluster)) 
  train_dat <- do.call(rbind, cipher_cluster)
  
  start_time <- Sys.time()
  dataset_predicted <- knn(train = train_dat, test = test_data, cl = train_lab, k = 2)
  end_time <- Sys.time()
  
  run_time <- end_time - start_time
  
  accuracy = mean(dataset_predicted == test_data_labels)
  return (c(accuracy, run_time))
}

find_best_center <- function() {
  findData <- do.call(rbind, idList[1:1]) 
  findData <- as.data.frame(findData)
  findData[,1] <- factor(findData[,1])
  
  id_train <- findData[,-1]
  id_train_labels <- findData[,1]
  
  test_dataSet <- do.call(rbind, idList[2:2]) 
  test_dataSet <- as.data.frame(test_dataSet)
  test_dataSet[,1] <- factor(test_dataSet[,1])
  
  id_test <- test_dataSet[,-1]
  id_test_labels <- test_dataSet[,1]
  
  center_vals <- c()
  acc_val <- c()
  
  for (i in 1:199) { #### WARNING takes a long time, 5 - 10 min
    res <- run_kmeans_and_knn(id_train, id_train_labels, id_test, id_test_labels, i)
    center_vals[i] <- i
    acc_val[i] <- res[1]
  }
  
  str(center_vals)
  str(acc_val)
  
  plot_data <- tibble(center_vals = center_vals, accuracy = acc_val)
  ggplot(data = plot_data, aes(x = center_vals))+
    geom_line(aes(y = accuracy))
  
}

###
# 3.1.1
###
find_best_center()

###
# 3.1.2
###
cross_validation_kmeans_and_knn <- function(data_set, seed) {
  set.seed(seed)
  folds <-createFolds(data_set[, 1], k = 10) #data_set$X1
  
  cross_validation_results <- lapply(folds, function(x) {
    
    #90% of the entire dataset is assigned to data_train
    data_train_with_labels <- data_set[-x, ]
    data_train <- data_train_with_labels[, -1]
    data_train_labels <- data_train_with_labels[ , 1]
    
    # The rest 10% of the dataset is assigned to data_test
    data_test_with_labels <- data_set[x, ]
    data_test <- data_test_with_labels[, -1]
    data_test_labels <- data_test_with_labels[ , 1]
    
    kmeans_res <- run_kmeans_and_knn(data_train, data_train_labels, data_test, data_test_labels, 6)
    
    #Create a knn
    start_time <- Sys.time()
    data_pred <- knn(train = data_train, test = data_test, cl = data_train_labels, k = 2)
    end_time <- Sys.time()
    
    run_time <- end_time - start_time
    
    accuracy = mean(data_pred == data_test_labels)
    
    return (c(kmeans_res[1], kmeans_res[2], accuracy, run_time))
    
  })
  return(cross_validation_results)
}


kmeans_and_knn_res <- cross_validation_kmeans_and_knn(dataSet, 123)
res <- do.call(rbind, kmeans_and_knn_res[])
res <- as.data.frame(res)

boxplot(res[[1]], res[[3]], names=c("with kmeans","raw data"))
boxplot(res[[2]], res[[4]], names=c("with kmeans","raw data"))

###
# 3.1.3
# Perform K-means clustering on each cipher individually for the training data from all the 
# available datasets ( disjunct ). Represent the training data as a number of cluster centroids and 
# compare performance, try multiple cluster sizes.
###
zeroes <- data.frame()
ones <- data.frame()
twos <- data.frame()
threes <- data.frame()
fours <- data.frame()
fives <- data.frame()
sixes <- data.frame()
sevens <- data.frame()
eights <- data.frame()
nines <- data.frame()

for (i in 1:13) {
  
  subject <- do.call(rbind, idList[i:i]) 
  subject <- as.data.frame(subject)
  subject[,1] <- factor(subject[,1])
  
  zeroes <- rbind(subject[1:200,], zeroes)
  ones <- rbind(subject[201:400,], ones)
  twos <- rbind(subject[401:600,], twos)
  threes <- rbind(subject[601:800,], threes)
  fours <- rbind(subject[801:1000,], fours)
  fives <- rbind(subject[1001:1200,], fives)
  sixes <- rbind(subject[1201:1400,], sixes)
  sevens <- rbind(subject[1401:1600,], sevens)
  eights <- rbind(subject[1601:1800,],eights)
  nines <- rbind(subject[1801:2000,], nines)

  }

#res <- run_kmeans_and_knn(id_train, id_train_labels, id_test, id_test_labels, i)
# TODO: Get from dataframes with cipher data to format that can be passed to kmeans function

#####################
## Exercise 3.3: Evaluation methods of k-NN
#####################
dataset_shuffle <-dataSet[sample(nrow(dataSet)),]

samp.train <- sample(nrow(dataSet), nrow(dataSet)*0.8)
data_train_with_labels <- dataSet[samp.train,,]
data_train <- data_train_with_labels[, -1]
data_train_labels <- data_train_with_labels[ , 1]

# The rest 10% of the dataset is assigned to data_test
data_test_with_labels <- dataSet[sample(setdiff(seq(nrow(dataSet)), samp.train), length(setdiff(seq(nrow(dataSet)), samp.train)) * 0.2),,]
data_test <- data_test_with_labels[, -1]
data_test_labels <- data_test_with_labels[ , 1]

###
# 3.3.1
###
allRec <- c()
allPre <- c()
allF1 <- c()
for(k in 1:13) {
  partRec <- c()
  partPre <- c()
  partF1 <- c()
  for(i in 1:k) {
    
    id_test_pred <- knn(train = data_train, test = data_test, cl = data_train_labels, k=k, l=i)# this is the task about: train 'l' up to 'k'
    
    cf <- confusionMatrix(data_test_labels, id_test_pred)
    
    #print( sum(diag(cf$table))/sum(cf$table) )
    
    trupL  <- sum(diag(cf$table))
    falpL <- sum(colSums(cf$table) - diag(cf$table))
    
    rec <-  trupL/length(id_test_pred)
    
    pre <- trupL/(trupL + falpL)
    
    f1 <- (2 * ((pre * rec) / (pre + rec)))
    
    partRec[[i]] <- rec
    partPre[[i]] <- pre
    partF1[[i]] <- f1
  }
  allRec[[k]] <-partRec
  allPre[[k]] <-partPre
  allF1[[k]] <-partF1
}

plot(allRec[[1]], allPre[[1]], type="b", col=1, lwd=1, pch=1, xlab="Recall", ylab="Precision",ylim=range(0.91,1),xlim=range(0.6,0.96))
plot_labels <- c("k = 1")
for (i in 2:13) {
  lines(allRec[[i]], allPre[[i]], type="b", col=i, lwd=1, pch=i)
  plot_labels[i] <- paste("k = ", i, sep = "")
}
title("Precision-recall curves for k 1 to 13")
legend("bottomleft",plot_labels, lwd=c(1), col=c(1:13), pch=c(1:13), y.intersp=1)

###
# 3.3.2
###
maxF1s = c()
for (i in 1:13) {
  maxF1s[[i]] <- max(unlist(allF1[[i]]))
}
plot(c(1:13), maxF1s, type="b", col=1, lwd=1, pch=1, xlab="K value", ylab="Max F1")
title("Maximum F1 values for each k")

# https://stackoverflow.com/questions/8499361/easy-way-of-counting-precision-recall-and-f1-score-in-r/8502026
# https://stackoverflow.com/questions/40783331/rocr-error-format-of-predictions-is-invalid
# https://stackoverflow.com/questions/61955696/calculating-true-false-positive-and-true-false-negative-values-from-matrix-in-r
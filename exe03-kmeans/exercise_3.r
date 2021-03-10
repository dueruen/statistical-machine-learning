library(stats)
library(class)
library(gmodels)
library(tibble)
library(ggplot2)
library(caret)

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
  
  dataset_predicted <- knn(train = train_dat, test = test_data, cl = train_lab, k = 2)
  
  accuracy = mean(dataset_predicted == test_data_labels)
  return (accuracy)
}

find_best_center <- function() {
  dataSet <- do.call(rbind, idList[1:1]) 
  dataSet <- as.data.frame(dataSet)
  dataSet[,1] <- factor(dataSet[,1])
  
  id_train <- dataSet[,-1]
  id_train_labels <- dataSet[,1]
  
  test_dataSet <- do.call(rbind, idList[2:2]) 
  test_dataSet <- as.data.frame(test_dataSet)
  test_dataSet[,1] <- factor(test_dataSet[,1])
  
  id_test <- test_dataSet[,-1]
  id_test_labels <- test_dataSet[,1]
  
  center_vals <- c()
  acc_val <- c()
  
  for (i in 1:199) { #### WARNING takes a long time, 5 - 10 min
    acc <- run_kmeans_and_knn(id_train, id_train_labels, id_test, id_test_labels, i)
    center_vals[i] <- i
    acc_val[i] <- acc
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
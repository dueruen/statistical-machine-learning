library(stats)
library(class)
library(gmodels)
library(tibble)
library(ggplot2)
library(caret)
library (ROCR)
library(dendextend)


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
# 3.2.1
### https://www.datacamp.com/community/tutorials/hierarchical-clustering-R
set.seed (2)
#Get one person from the dataset, so idList[1:1] 
one <- do.call(rbind, idList[1:1]) 
one <- as.data.frame(one)
one[,1] <- factor(one[,1])


#Empty list 
digit.list <- list()

#Add each 5 instances of each digits to digit.list
for( i in 0:9) { 
  tmp.digit <- one[(2+(i*200)):(6 + (i*200)),]
  digit.list <- append(digit.list, list(tmp.digit))
}

par(mfrow=c(1,3))
#Convert digit.list to a matrix
digit.mat <- do.call(rbind, digit.list)
digit.mat <- as.data.frame(digit.mat)
digit.mat[,1] <- factor(digit.mat[,1])

dist_mat <- dist(digit.mat, method = 'euclidean')
cluster.labels <- digit.mat[,1]

# Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.
hclust.avg <- hclust(dist_mat, method = 'average')
cut_avg <- cutree(hclust.avg, k = 10)
plot(hclust.avg, labels = cluster.labels, 
     main="HC with 5 instances of each digit for one person", xlab="", sub="",ylab="")
#Color boxes
rect.hclust(hclust.avg , k = 6, border = 2:6)


###
# 3.2.2 - Use K-Means clustering to compress each digit into 5 clusters, as done in 3.1.1, 
# and perform hierarchical clustering to show a low level dendrogram of this ( one person )
###
set.seed (2)
#Perform Perform k-means clustering on a data matrix.
clusterData <- kmeans(dist_mat, centers = 5) 
#Take the cluster data and compute matrix
cluster <- dist(clusterData$cluster)
hc.average <- hclust(cluster, method = 'average')

plot(hc.average, labels = cluster.labels, 
     main="HC with centroids for each digit for one person", xlab="", sub="",ylab="")
#Color boxes
rect.hclust(hc.average , k = 5, border = 2:6)


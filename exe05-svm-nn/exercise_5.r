library(rpart)
library(rpart.plot) #SKAL INSTALLERES
library(stats)
library(randomForest)
library(caret)



#Load the dataset
load("idList-cornered-100-2021.Rdata")

#take one person from the dataset
dataset.oneperson <- do.call(rbind,idList[1:1])
dataset.oneperson <- as.data.frame(dataset.oneperson)
dataset.oneperson[,1] <- factor(dataset.oneperson[,1])
dataset.oneperson.no_labels <- dataset.oneperson[,-1]

#########
## 4.1.1
## Compute the optimal decision point for the first 5 PCAs of a dataset (e.g. a single person) and 
## compute the information gain associated to it (plot 5 graphs, one for each component, and show the highest information gain)
#########

performPCA <- function(dataset, pcaCount){
  pca <- prcomp(dataset,scale=FALSE, rank. = pcaCount)
  return (pca)
}

#Transform data
transform.data <- function(dataset, pca){
  transformed.dataset <- predict(pca, newdata = dataset)
  # Convert to transformed dataset to dataframe
  df.transformed.dataset <- as.data.frame(transformed.dataset) 
  return (df.transformed.dataset)
}

#Calculate entropy
entropy <- function(dataset){
  fullSum <- 0
  for (i in (0:9)) {
    if(nrow(dataset) > 0){
      pi <- nrow(dataset[dataset[,1] == i , ] / nrow(dataset))
    }else {
      pi <- 0
    }
    
    if(pi > 0){
      fullSum <- fullSum - pi * log2(pi)
    }
  }
  return(fullSum)
}

decisionPoint <- function(id_pca){
  entList <- c()
  entBefore <- entropy(dataset.oneperson)
  Pts <- seq(min(id_pca), max(id_pca), length.out=200)
  for( splitP in (1:200)){
    S1 <- dataset.oneperson.no_labels[ id_pca < Pts[splitP], ] # Perform splits
    S2 <- dataset.oneperson.no_labels[ id_pca >= Pts[splitP], ]
    s1 <- nrow(S1)
    s2 <- nrow(S2)
    ent <- ( s1 * entropy(S1) )/(s1 + s2) + ( s2 * entropy(S2) )/(s1 + s2) # Calculate entropy
    entList[splitP] <- entBefore - ent # Information gain is calculated
  }
  return(entList)
}

plotInformationGain <- function() {
  #Perform PCA but with only with 5 principal components
  pca.5 <- performPCA(dataset.oneperson.no_labels, 5)
  
  infoGainList <- c()
  
  for( i in (1:5)) { 
    pca_i <- pca.5$x[, i]
    infoGainList[[i]] <- decisionPoint(pca_i)
  }
  
  plot(c(1:200), infoGainList[[1]], type="b", col=1, lwd=i, pch=1, xlab="Split", ylab="InfoGain")
  plot_labels <- c("PC 1")
  for (i in 2:5) {
    lines(c(1:200), infoGainList[[i]], type="b", col=i, lwd=1, pch=i)
    plot_labels[i] <- paste("PC ", i, sep = "")
  }
  title("Information Gain development from split 1 to 200")
  legend("bottomleft",plot_labels, lwd=c(1), col=c(1:200), pch=c(1:200), y.intersp=1)
}

# WARNING: 5-10 minute-ish execution time
plotInformationGain()


###########
## 4.1.2 - DONE
## Compute a decision tree for the digit classification and visualize it.
###########
df.transformed.dataset <- transform.data(dataset.oneperson, pca.5)
datanew <- cbind(df.transformed.dataset, dataset.oneperson[,1]) # 'df.transformed.dataset' is a dataframe
datanew$States <- factor(datanew[,6])

#Create decision tree and limit the depth.
tree <- rpart(States ~ PC1 + PC2 + PC3 + PC4 + PC5, data = datanew, 
              method = "class", control = list(maxdepth = 5))
#Plot decision tree
rpart.plot(tree, box.palette = 0) #Box palette for ignoring error

###########
## 4.1.3 - DONE
## Using the full data set(i.e. dataset from multiple people), 
## evaluate a trained decision tree using cross validation. 
## Try to train a tree with PCA, and without PCA(raw data).Discuss the important parameters.
###########
cross_validation_with_pca <- function(data_set, seed) {
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
    
    pca.5 <- performPCA(data_train, 5)
    
    df.transformed.dataset <- transform.data(data_train_with_labels, pca.5)
    datanew <- cbind(df.transformed.dataset, data_train_with_labels[,1]) # 'df.transformed.dataset' is a dataframe
    datanew$States <- factor(datanew[,6])
    
    start_time <- Sys.time()
    tree <- rpart(States ~ ., data = datanew, 
                  method = "class", control = list(maxdepth = 5))
    end_time <- Sys.time()
    
    #Predict random vs. test set
    df.transformed.test_dataset <- transform.data(data_test_with_labels, pca.5)
    datanew_test <- cbind(df.transformed.test_dataset, data_train_with_labels[,1]) # 'df.transformed.dataset' is a dataframe
    datanew_test$States <- factor(datanew_test[,6])
    
    data_pred <-predict(tree, datanew_test, type = "class")
    
    run_time <- end_time - start_time
    
    accuracy = mean(data_pred == data_test_labels)
    #accuracy = 1
    
    #crossTable <- CrossTable(x = data_test_labels, y = data_pred, prop.chisq = FALSE)
    
    return (c(accuracy, run_time))
    
  })
  return(cross_validation_results)
}

cross_validation_without_pca <- function(data_set, seed) {
  set.seed(seed)
  folds <-createFolds(data_set[, 1], k = 10) #data_set$X1
  
  cross_validation_results <- lapply(folds, function(x) {
    
    #90% of the entire dataset is assigned to data_train
    data_train_with_labels <- data_set[-x, ]
    data_train <- data_train_with_labels[, -1]
    data_train_labels <- data_train_with_labels[ , 1]
    #cat(nrow(data_train_with_labels))
    #cat(" ")
    #cat(data_train_with_labels$V1[1])
    
    # The rest 10% of the dataset is assigned to data_test
    data_test_with_labels <- data_set[x, ]
    data_test <- data_test_with_labels[, -1]
    data_test_labels <- data_test_with_labels[ , 1]
    #cat(nrow(data_test_with_labels))
    #cat(" ")
    #cat(data_test_with_labels$V1[1])
    #cat("\n")
    
    start_time <- Sys.time()
    tree <- rpart(V1 ~ ., data = data_train_with_labels, 
                     method = "class", control = list(maxdepth = 5))
    end_time <- Sys.time()
    
    #Predict random vs. test set
    data_pred <-predict(tree, data_test, type = "class")
    
    run_time <- end_time - start_time
    
    accuracy = mean(data_pred == data_test_labels)
    #accuracy = 1
    
    #crossTable <- CrossTable(x = data_test_labels, y = data_pred, prop.chisq = FALSE)
    
    return (c(accuracy, run_time))
    
  })
  return(cross_validation_results)
}
res_cross_validation_with_pca <- cross_validation_with_pca(dataset_shuffle, 123)

res_cross_validation_without_pca <- cross_validation_without_pca(dataset_shuffle, 123)

res_with_pca <- do.call(rbind, res_cross_validation_with_pca[])
res_with_pca <- as.data.frame(res_with_pca)

res_withOut_pca <- do.call(rbind, res_cross_validation_without_pca[])
res_withOut_pca <- as.data.frame(res_withOut_pca)

boxplot(res_with_pca[[1]], res_withOut_pca[[1]], names=c("Using first 5 PCA's","Raw data"), main="Accuracy")
boxplot(res_with_pca[[2]], res_withOut_pca[[2]], names=c("Using first 5 PCA's","Raw data"), main="Run time in s")


###########
## 4.2.1 - Random forests WORK IN PROGRESS
## Create a Random Forest classifier and evaluate it using cross validation.
## Discuss the critical parameters of “randomForest”(e.g., number and depth of trees)
###########

#Take one person and transform dataset and add States column i think?
df.transformed.dataset <- transform.data(dataset.oneperson, pca.5)
datanew <- cbind(df.transformed.dataset, dataset.oneperson[,1]) # 'df.transformed.dataset' is a dataframe
datanew$States <- factor(datanew[,6])

# Create Random Forest classifer. Uses single person. This should be correct.
model <- randomForest(States ~ PC1 + PC2 + PC3 + PC4 + PC5, data = datanew, ntree=100)

# Logging to get to know random forest
model
# Using the importance() function, we can view the importance of each variable.
importance(model)

# WIP
cross_validation_random_forest <- function(data_set, model, seed) {
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
    
    
    #Predict random forest vs. test set
    start_time <- Sys.time()
    data_pred <-predict(model,data_test)
    end_time <- Sys.time()
    
    run_time <- end_time - start_time
    
    accuracy = mean(data_pred == data_test_labels)
    
    #crossTable <- CrossTable(x = data_test_labels, y = data_pred, prop.chisq = FALSE)
    
    return (c(accuracy, run_time))
    
  })
  return(cross_validation_results)
}

cross_validation_random_forest(datanew,model,2)

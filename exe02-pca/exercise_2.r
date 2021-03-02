#Packages
library(stats)
library(gmodels)
library(class)
library(caret)
library(e1071)
library(ggplot2)
library(irr)

#Don't print numbers as exponential notation
options("scipen"=100, "digits"=4)

#Load the dataset
load("idList-co-100.rdata")

#
idLoaded <- do.call(rbind, idList[1:10])
idLoaded <- as.data.frame(idLoaded)
idLoaded[,1] <- factor(idLoaded[,1])

dataset_no_labels <- idLoaded[,-1]

# 2.1.1 - Show the standard deviation ( From prcompEigenvalues ), the proportion of variance 
# and the cumulative sum of variance of the principal components.
#Perform PCA on the dataset, and scale=TRUE because the variables should have standard deviation one. This is advisable.
pca <- prcomp(dataset_no_labels,scale=TRUE)

#Show the summary with standard deviation, proportion of variance and Cumulative Proportion
summary(pca)

#The variance explained by each principal component is obtained by squaring these:
pr.var=pca$sdev^2
pr.var
#To compute the proportion of variance explained by each principal component, 
# we simply divide the variance explained by each principal component by the total variance explained by all four principal components:
pve=pr.var/sum(pr.var)
pve

#24 PCA represent 80% of the accumulated variance
sum(pve[1:24])

#38 PCA represent 90% of the accumulated variance
sum(pve[1:38])

#54 PCA represent 95% of the accumulated variance
sum(pve[1:54])

#98 PCA represent 99% of the accumulated variance
sum(pve[1:98])

#A plot of the PVE explained by each component
plot(pve[1:100], xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type="b")
abline(v = 5, col="red", lty=5)
#the cumulative PVE plotted. This shows the CDF and tells us when x% of variance is explained. Only take the first 100 PCA's
plot(cumsum(pve[1:100]), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type="b")
abline(h = 0.8, col="red", lty=5)
abline(h = 0.9, col="green", lty=5)
abline(h = 0.95, col="blue", lty=5)
abline(h = 0.99, col="yellow", lty=5)

# Exercise 2.2 - Normalization

# Z-standardization
dataset_z <- as.data.frame(scale(idLoaded[-1]))

# The mean of a z-score standardized variable should always be zero, and the range should be fairly compact. 
# A z-score greater than 3 or less than -3 indicates an extremely rare value. The previous summary seems reasonable.
summary(dataset_z$V2)

#  apply kNN with 10 fold cross-validation (10 runs, 90% training and 10% test set).
cross_validation <- function(data_set, seed) {
  set.seed(seed)
  folds <-createFolds(data_set[, 1], k = 10) #data_set$X1
  
  #lapply() returns a list of the same length as X, each element of which is the result 
  # of applying FUN to the corresponding element of X
  cross_validation_results <- lapply(folds, function(x) {
    
    #90% of the entire dataset is assigned to data_train
    data_train_with_labels <- data_set[-x, ]
    data_train <- data_train_with_labels[, -1]
    data_train_labels <- data_train_with_labels[ , 1]
    
    # The rest 10% of the dataset is assigned to data_test
    data_test_with_labels <- data_set[x, ]
    data_test <- data_test_with_labels[, -1]
    data_test_labels <- data_test_with_labels[ , 1]
    
    #Create a knn
    data_pred <- knn(train = data_train, test = data_test, cl = data_train_labels, k = 63)
    
    #Kappa is used to compare performance in machine learning
    kappa <- kappa2(data.frame(data_test_labels, data_pred))$value
    return(kappa)
  })
  return(cross_validation_results)
}

#Dont know if broken - it takes long time
cross_validation(dataset_z,423)



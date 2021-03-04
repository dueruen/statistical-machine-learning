#Packages
library(stats)
library(gmodels)
library(class)
library(caret)
library(e1071)
library(ggplot2)
library(irr)
library(tibble)
library("spatstat")

#Don't print numbers as exponential notation
options("scipen"=100, "digits"=4)

#Load the dataset
load("idList-cornered-100-2021.Rdata")

#
idLoaded <- do.call(rbind, idList[1:13])
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
#sum(pve[1:24])

#38 PCA represent 90% of the accumulated variance
#sum(pve[1:38])

#54 PCA represent 95% of the accumulated variance
#sum(pve[1:54])

#98 PCA represent 99% of the accumulated variance
#sum(pve[1:98])

#A plot of the PVE explained by each component
plot(pve[1:100], xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type="b")
abline(v = 5, col="red", lty=5)
#the cumulative PVE plotted. This shows the CDF and tells us when x% of variance is explained. Only take the first 100 PCA's
plot(cumsum(pve[1:100]), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type="b")
abline(h = 0.8, col="red", lty=5)
abline(h = 0.9, col="green", lty=5)
abline(h = 0.95, col="blue", lty=5)
abline(h = 0.99, col="yellow", lty=5)

# 2.1.2
acc_vals <- c(0.80, 0.90, 0.95, 0.99)
k_vals <- c(1, 3, 5)

accuracy_all <- list(c(k_vals))
error_all <- list(c(k_vals))
time_all <- list(c(k_vals))
count = 0

set.seed(1234) # reproducibility
samp.train <- sample(nrow(idLoaded), nrow(idLoaded)*0.8)
df.train <- idLoaded[samp.train,,]
samp.test <- sample(setdiff(seq(nrow(idLoaded)), samp.train), length(setdiff(seq(nrow(idLoaded)), samp.train)) * 0.2)
df.test <- idLoaded[samp.test,]

df.train.data <- df.train[,-1]
df.train.labels <- df.train[,1]
df.test.data <- df.test[,-1]
df.test.labels <- df.test[,1]

for (a in acc_vals) {
  i <- 0
  accuracy <- c()
  error <- c()
  time <- c()
  
  for (k in k_vals){
    str(a)
    str(k)
    pca <- prcomp(df.train.data, scale = FALSE) 
    
    df.train.p <- predict(pca, newdata = df.train.data)[, cumsum(pve) <= a]
    
    df.test.p <- predict(pca, newdata = df.test.data)[, cumsum(pve) <= a]
    
    start_time <- Sys.time()
    test.preds.pca <- knn(train = df.train.p, test = df.test.p, cl = df.train.labels, k = k)
    end_time <- Sys.time()
    
    accuracy.k5 = mean(test.preds.pca == df.test.labels)
    str(accuracy.k5)
    error.rate.k5 = mean(test.preds.pca != df.test.labels)
    str(error.rate.k5)
    
    run_time <- end_time - start_time
    str(run_time)
    
    accuracy <- c(accuracy, accuracy.k5)
    error <- c(error, error.rate.k5)
    time <- c(time, run_time)
    i <- i + 1
  }
  count <- count + 1
  accuracy_all[[count + 1]] <- accuracy
  error_all[[count + 1]] <- error
  time_all[[count + 1]] <- time
}

tr <- tibble(kVals = accuracy_all[[1]], accuracy = accuracy_all[[2]], y = accuracy_all[[3]], z = accuracy_all[[4]], q = accuracy_all[[5]])
tr_error <- tibble(kVals = error_all[[1]], errorRate = error_all[[2]], y = error_all[[3]], z = error_all[[4]], q = error_all[[5]])
tr_time <- tibble(kVals = time_all[[1]], runTime = time_all[[2]], y = time_all[[3]], z = time_all[[4]], q = time_all[[5]])

ggplot(data = tr, aes(x = kVals))+
  geom_line(aes(y = accuracy, color = "80%")) + 
  geom_line(aes(y = y, color="90%")) +
  geom_line(aes(y = z, color="95%")) +
  geom_line(aes(y = q, color="99%"))

ggplot(data = tr_error, aes(x = kVals))+
  geom_line(aes(y = errorRate, color = "80%")) + 
  geom_line(aes(y = y, color="90%")) +
  geom_line(aes(y = z, color="95%")) +
  geom_line(aes(y = q, color="99%"))

ggplot(data = tr_time, aes(x = kVals))+
  geom_line(aes(y = runTime, color = "80%")) + 
  geom_line(aes(y = y, color="90%")) +
  geom_line(aes(y = z, color="95%")) +
  geom_line(aes(y = q, color="99%"))


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


#Exercise 2.3

# The code from the assignment document, which apparently performs  Gaussian smoothing on the data 
summary(idLoaded)
id_mat <- data.matrix(idLoaded, rownames.force = NA)
imageSize <- sqrt(ncol(id_mat) - 1)
rotate <- function(x) t(apply(x, 2, rev))

smoothImage <- function(grayImg){
  smoothed <- as.matrix(blur(as.im(grayImg), sigma = 0.5, normalise=FALSE, bleed = TRUE, varcov=NULL))
  return(smoothed)
}
 
# Smooth all images
for(i in 1:nrow(id_mat)) {
  rotated <- c(id_mat[i,2:ncol(idLoaded)])
  image <- matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
  image <- smoothImage(image)
  id_mat[i,2:ncol(id_mat)] <- matrix(image,nrow = 1,ncol = ncol(id_mat) - 1, byrow = FALSE)
}
idSmoothed <- as.data.frame(id_mat)
idSmoothed[,1] <- factor(idLoaded)
#summary(idSmoothed) #Adding this will almost crash R to output it. Just be patient, it usually works out
 
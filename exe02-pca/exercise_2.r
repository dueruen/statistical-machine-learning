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

# Data to dataframe
idLoaded <- do.call(rbind, idList[1:13]) 
idLoaded <- as.data.frame(idLoaded)
idLoaded[,1] <- factor(idLoaded[,1])

# Remove factor column
dataset_no_labels <- idLoaded[,-1]

#####################
## Exercise 2.1 -  Principal Component Analysis (PCA)
#####################

# 2.1.1 - Show the standard deviation ( From prcompEigenvalues ), the proportion of variance 
# and the cumulative sum of variance of the principal components.
#Perform PCA on the dataset, and scale=TRUE because the variables should have standard deviation one. This is advisable.
performPCA <- function(dataset){
  pca <- prcomp(dataset,scale=FALSE)
  return (pca)
}

pca <- performPCA(dataset_no_labels)

#Show the summary with standard deviation, proportion of variance and Cumulative Proportion
summary(pca)

calculateExplainedVariance <- function(pca){
  #The variance explained by each principal component is obtained by squaring these:
  pr.var=pca$sdev^2 
  return (pr.var)
}
pr.var <- calculateExplainedVariance(pca)
#To compute the proportion of variance explained by each principal component, 
# we simply divide the variance explained by each principal component by the total variance explained by all four principal components:
calculatePropOfVariance <- function(pr.var){
  pve=pr.var/sum(pr.var)
  return (pve)
}

pve <- calculatePropOfVariance(pr.var)
pve
#24 PCA represent 80% of the accumulated variance
#sum(pve[1:24])

#38 PCA represent 90% of the accumulated variance
#sum(pve[1:38])

#54 PCA represent 95% of the accumulated variance
#sum(pve[1:54])

#98 PCA represent 99% of the accumulated variance
#sum(pve[1:98])

# A plot of the variance
plot(pr.var[1:70], xlab="Principal Component", ylab="Variance", ylim=c(0,40),type="b")
abline(v = 8, col="green", lty=5)
abline(v = 67, col="red", lty=5)

#A plot of the PVE explained by each component
plot(pve[1:20], xlab="Principal Component", ylab="Proportion of Variance Explained",type="b")
abline(v = 8, col="red", lty=5)
#the cumulative PVE plotted. This shows the CDF and tells us when x% of variance is explained. Only take the first 100 PCA's
plot(cumsum(pve[1:100]), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type="b")
abline(h = 0.8, col="red", lty=5)
abline(h = 0.9, col="green", lty=5)
abline(h = 0.95, col="blue", lty=5)
abline(h = 0.99, col="yellow", lty=5)

# 2.1.2
checkPerformance <- function(traingDataSet, testDataSet) {
  # The PC accuracy percentages 
  acc_vals <- c(0.80, 0.90, 0.95, 0.99)
  
  # K values to test
  k_vals <- c(2, 3, 4, 7)
  
  # List to store the results in
  accuracy_all <- list(c(k_vals))
  error_all <- list(c(k_vals))
  time_all <- list(c(k_vals))
  
  # Count value used to set values in lists
  count = 0
  
  # The training data no without label
  df.train.data <- traingDataSet[,-1]
  # The training data label
  df.train.labels <- traingDataSet[,1]
  
  # The test data no without label
  df.test.data <- testDataSet[,-1]
  # The training data label
  df.test.labels <- testDataSet[,1]
  
  # Loop going through each accuracy value
  for (a in acc_vals) {
    # Used to store the results for the accuracy value
    accuracy <- c()
    error <- c()
    time <- c()
    
    # Loop testing each K value
    for (k in k_vals){
      str(a)
      str(k)
      
      # Make pca
      pca <- prcomp(df.train.data, scale = FALSE) 
      
      # The number of PC to include
      in_pc <- cumsum(pve) <= a
      
      # Create the new training set based using pcq
      df.train.p <- predict(pca, newdata = df.train.data)[, in_pc]
      
      # Create the new test set based using pca
      df.test.p <- predict(pca, newdata = df.test.data)[, in_pc]
      
      # KNN on the new dataset
      start_time <- Sys.time()
      test.preds.pca <- knn(train = df.train.p, test = df.test.p, cl = df.train.labels, k = k)
      end_time <- Sys.time()
      
      # Calc the accuracy of knn
      accuracy.k5 = mean(test.preds.pca == df.test.labels)
      str(accuracy.k5)
      
      # Calc the error rate of knn
      error.rate.k5 = mean(test.preds.pca != df.test.labels)
      str(error.rate.k5)
      
      run_time <- end_time - start_time
      str(run_time)
      
      accuracy <- c(accuracy, accuracy.k5)
      error <- c(error, error.rate.k5)
      time <- c(time, run_time)
    }
    count <- count + 1
    accuracy_all[[count + 1]] <- accuracy
    error_all[[count + 1]] <- error
    time_all[[count + 1]] <- time
  }
  
  tr <- tibble(kVals = accuracy_all[[1]], accuracy = accuracy_all[[2]], y = accuracy_all[[3]], z = accuracy_all[[4]], q = accuracy_all[[5]])
  tr_error <- tibble(kVals = error_all[[1]], errorRate = error_all[[2]], y = error_all[[3]], z = error_all[[4]], q = error_all[[5]])
  tr_time <- tibble(kVals = time_all[[1]], runTime = time_all[[2]], y = time_all[[3]], z = time_all[[4]], q = time_all[[5]])
  
  return (list(tr, tr_error, tr_time))
}

# Disjunct 
# Comment this out or the All persons in 
set.seed(1234) # reproducibility
samp.train <- sample(nrow(idLoaded), nrow(idLoaded)*0.8)
df.train <- idLoaded[samp.train,,]
samp.test <- sample(setdiff(seq(nrow(idLoaded)), samp.train), length(setdiff(seq(nrow(idLoaded)), samp.train)) * 0.2)
df.test <- idLoaded[samp.test,]

pro_res <- checkPerformance(df.train, df.test)

# All persons in
# Comment this out or the Disjunct 
set.seed(1234) # reproducibility
dataset_shuffle <-idLoaded[sample(nrow(idLoaded)),]  
samp.train <- sample(nrow(dataset_shuffle), nrow(dataset_shuffle)*0.8)
df.train <- dataset_shuffle[samp.train,,]
samp.test <- sample(setdiff(seq(nrow(dataset_shuffle)), samp.train), length(setdiff(seq(nrow(dataset_shuffle)), samp.train)) * 0.2)
df.test <- dataset_shuffle[samp.test,]

pro_res <- checkPerformance(df.train, df.test)


ggplot(data = pro_res[[1]], aes(x = kVals))+
  geom_line(aes(y = accuracy, color = "80%")) + 
  geom_line(aes(y = y, color="90%")) +
  geom_line(aes(y = z, color="95%")) +
  geom_line(aes(y = q, color="99%"))

ggplot(data = pro_res[[2]], aes(x = kVals))+
  geom_line(aes(y = errorRate, color = "80%")) + 
  geom_line(aes(y = y, color="90%")) +
  geom_line(aes(y = z, color="95%")) +
  geom_line(aes(y = q, color="99%"))

ggplot(data = pro_res[[3]], aes(x = kVals))+
  geom_line(aes(y = runTime, color = "80%")) + 
  geom_line(aes(y = y, color="90%")) +
  geom_line(aes(y = z, color="95%")) +
  geom_line(aes(y = q, color="99%"))


#####################
## Exercise 2.2 - Normalization
#####################

# Apply normalization(Z-standardization) 
dataset_z <- as.data.frame(scale(idLoaded[-1]))

# The mean of a z-score standardized variable should always be zero, and the range should be fairly compact. 
# A z-score greater than 3 or less than -3 indicates an extremely rare value. The previous summary seems reasonable.
summary(dataset_z$V2)

#  apply kNN with 10 fold cross-validation (10 runs, 90% training and 10% test set).
cross_validation <- function(data_set, seed, beforePCA) {
  set.seed(seed)
  #Normalize data before PCA
  if (beforePCA) {
    data_set <- as.data.frame(scale(data_set[-1])) 
  }
  
  folds <-createFolds(data_set[, 1], k = 10) #data_set$X1
  
  #lapply() returns a list of the same length as X, each element of which is the result 
  # of applying FUN to the corresponding element of X
  cross_validation_results <- lapply(folds, function(x) {
    
    #90% of the entire dataset is assigned to data_train
    data_train_with_labels <- data_set[-x, ]
    df.train.data <- data_train_with_labels[, -1]
    df.train.labels <- data_train_with_labels[ , 1]
    
    # The rest 10% of the dataset is assigned to data_test
    data_test_with_labels <- data_set[x, ]
    df.test.data <- data_test_with_labels[, -1]
    data_test_labels <- data_test_with_labels[ , 1]
    
    #Perform PCA on normalized dataset
    pca.z <- performPCA(df.train.data)
    
    pr.var.z <- calculateExplainedVariance(pca.z)
    pve.z <- calculatePropOfVariance(pr.var.z)
    
    # The number of PC to include
    in_pc <- cumsum(pve.z) <= 0.8
    
    # Create the new training set based using pcq
    df.train.p <- predict(pca.z, newdata = df.train.data)[, in_pc]
    # Create the new test set based using pca
    df.test.p <- predict(pca.z, newdata = df.test.data)[, in_pc]
    
    if(!beforePCA){
      df.train.p <- as.data.frame(scale(df.train.p)) 
      df.test.p <- as.data.frame(scale(df.test.p))
    }
    
    # KNN on the new dataset
    start_time <- Sys.time()
    test.preds.pca <- knn(train = df.train.p, test = df.test.p, cl = df.train.labels, k = 3)
    end_time <- Sys.time()
    
    #Print run time
    run_time <- end_time - start_time
    str(run_time)
    
    # Calc the accuracy of knn
    accuracy = mean(test.preds.pca == data_test_labels)
    str(accuracy)
    
    # Calc the error rate of knn
    error.rate = mean(test.preds.pca != data_test_labels)
    str(error.rate)
    return(list(accuracy, error.rate, run_time))
  })
  return(cross_validation_results)
}

cross_validation_and_print <- function (data_set, seed, beforePCA) {
  cross_val_res <- cross_validation(data_set, seed, beforePCA)
  
  #Examine the results
  #str(cross_val_res)
  
  tt <- do.call(rbind, cross_val_res[1:10])
  #Calculate mean of these 10 results while cross_validation_results is not a numeric vector, 
  # i'll have to do unlist() which creates a numeric vector where i can use mean() 
  cat(" The accuracy is:", mean(unlist(tt[,1])))
  cat(" The error rate is:", mean(unlist(tt[,2])))
  cat(" The run time is:", mean(unlist(tt[,3])))
}

cross_validation_and_print(idLoaded, 423, beforePCA = FALSE)
cross_validation_and_print(dataset_z, 423, beforePCA = TRUE)

#####################
## Exercise 2.3 - Preprocessing
#####################

# The code from the assignment document, which apparently performs  Gaussian smoothing on the data 
summary(idLoaded)
sigma_value <- 0.5 #Controls the amount of variation allowed around the mean value during the smoothing, and thus the degree of smoothing
id_mat <- data.matrix(idLoaded, rownames.force = NA)
imageSize <- sqrt(ncol(id_mat) - 1)
rotate <- function(x) t(apply(x, 2, rev))

#Gaussian smoothing function
smoothImage <- function(grayImg, sigma_value){
  smoothed <- as.matrix(blur(as.im(grayImg), sigma = sigma_value, normalise=FALSE, bleed = TRUE, varcov=NULL))
  return(smoothed)
}

# Smooth all images
for(i in 1:nrow(id_mat)) {
  rotated <- c(id_mat[i,2:ncol(idLoaded)])
  image <- matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
  image <- smoothImage(image, sigma_value)
  id_mat[i,2:ncol(id_mat)] <- matrix(image,nrow = 1,ncol = ncol(id_mat) - 1, byrow = FALSE)
}
idSmoothed <- as.data.frame(id_mat)
idSmoothed[,1] <- factor(idLoaded) #idSmoothed holds the smoothed image data 

#Cross-validation with normalization before PCA is done on dataset
cross_validation_and_print(idSmoothed, 423, beforePCA = TRUE)

#Cross-validation with normalization after PCA is done on dataset
cross_validation_and_print(idSmoothed, 423, beforePCA = FALSE)
 
#####################
## Exercise 2.4 - Reconstruction using PCA
#####################

# 2.4.1 - This task is about reconstructing data using PCA. 
# First using these functions we can plot an image of a single cipher 
# ( for plotting images do not convert idto data frame ):
plotCipherImage <- function(cipher, rawDataset){
  id_mat <-data.matrix(rawDataset, rownames.force = NA)
  rotate <-function(x) t(apply(x,2, rev))
  imageSize <-sqrt(ncol(id_mat) -1)
  # Plot an image of a single cipher - To change the cipher to plot, change the for loop range
  #for(i in 1:cipher + 1){
  rotated <-c(id_mat[-200+(cipher + 1)*200+1,2:ncol(id_mat)])
  rotated <-((rotated -min(rotated)) / (max(rotated) -min(rotated)))
  image <-matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
  image <-rotate(image)
  image( image,  zlim=c(0,1), col=gray(0:100/100) )
  #} 
}

plotCipherImage(2,idLoaded)

# 2.4.2. - Plot the first 10 eigenvectors/loadingvectors as images. Can you describe what you see?
plotEigen <- function (num) {
  id_mat <-data.matrix(pca$rotation[,num], rownames.force = NA)
  rotate <-function(x) t(apply(x,2, rev))
  imageSize <-sqrt(nrow(id_mat))
  
  rotated <-c(id_mat[])
  rotated <-((rotated -min(rotated)) / (max(rotated) -min(rotated)))
  image <-matrix(rotated ,nrow = imageSize ,ncol = imageSize, byrow = FALSE)
  image <-rotate(image)
  image( image,  zlim=c(0,1), col=gray(0:100/100) )
}

plotEigen(1)

# 2.4.3
plotReconstructionCipherImage <- function(cipher, varianceProcent){
  trunc <- pca$x[-200+(cipher+1)*200+1,cumsum(pve) < varianceProcent] %*% t(pca$rotation[,cumsum(pve) < varianceProcent])
  trunc <- scale(trunc, center = -1 * pca$center, scale=FALSE)
  id_mat <- trunc
  rotate <-function(x) t(apply(x,2, rev))
  imageSize <-sqrt(ncol(id_mat))
  
  rotated <-c(id_mat[])
  rotated <-((rotated -min(rotated)) / (max(rotated) -min(rotated)))
  image <-matrix(rotated,nrow = imageSize,ncol = imageSize, byrow = FALSE)
  image <-rotate(image)
  image( image,  zlim=c(0,1), col=gray(0:100/100) )
}

plotReconstructionCipherImage(7, 0.99)

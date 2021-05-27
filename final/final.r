library(rpart)
library(rpart.plot) #SKAL INSTALLERES
library(stats)
library(randomForest)
library(caret)
library(kernlab)
library(gmodels)
library(class)
library(caret)
library("spatstat")

#######
#Load the dataset
#######
load("idList-FinalExam.Rdata")

id <- do.call(rbind, idList[])
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

items_per_person = nrow(id) / length(idList)
raw_disjunct_train <- id[1:(items_per_person*30),]
raw_disjunct_test <- id[(items_per_person*30 + 1):(items_per_person*38),]

set.seed(1234)
dataset_shuffle <-id[sample(nrow(id)),]
raw_all_persons_train <- dataset_shuffle[1:(items_per_person*30),]
raw_all_persons_test <- dataset_shuffle[(items_per_person*30 + 1):(items_per_person*38),]

#######
## Plot raw data
#######
plotData <- function (data_to_plot, persons) {
  img <- matrix(,180,18*persons)
  
  for (x in 0:10){
    idx <- 0
    for (y in 0:persons) {
      idx <- idx+1
      d <- data_to_plot[[y + 1]]
      tmpM <- matrix(d[(x*200),2:325],18,18)
      for (xM in 1:18) {
        for (yM in 1:18) {
          img[(x-1)*18+xM, (y-1)*18+yM] <- tmpM[xM,yM]
        }
      }
    }
  }
  rotate <- function(x) t(apply(x, 2, rev))
  image(rotate(img),col=gray(0:100/100) )
}

plotData (idList, 35)

##########
##########
## Preprocessing raw data
##########
##########
plotData2 <- function (data_to_plot) {
  id_mat <-data.matrix(data_to_plot, rownames.force = NA)
  img <- matrix(,180,180)
  cipher <- 1
  for (x in 1:10){
    for (y in 1:10) {
      rotated <-c(id_mat[-200+((x)*200)+1+(y*2000),2:ncol(id_mat)])
      rotated <-((rotated -min(rotated)) / (max(rotated) -min(rotated)))
      tmpM <-matrix(rotated,18,18)
      
      for (xM in 1:18) {
        for (yM in 1:18) {
          img[(x-1)*18+xM, (y-1)*18+yM] <- tmpM[xM,yM]
        }
      }
    }
  }
  rotate <- function(x) t(apply(x, 2, rev))
  image(rotate(img),col=gray(0:100/100) )
}

#######
## Min max normalization
#######
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

id_min_max_normalized <- as.data.frame(lapply(id[-1], normalize))
id_min_max_normalized <- cbind(V1 = id[1], id_min_max_normalized)

plotData2 (id_min_max_normalized)

#######
## Z normalization
#######
id_z_normalized <- as.data.frame(scale(id[-1]))
id_z_normalized <- cbind(V1 = id[1], id_z_normalized)

plotData2 (id_z_normalized)

#######
## Gaussian smoothing
#######
smoothImage <- function(grayImg, sigma_value){
  smoothed <- as.matrix(blur(as.im(grayImg), sigma = sigma_value, normalise=FALSE, bleed = TRUE, varcov=NULL))
  return(smoothed)
}

gaussianSmoothing <- function (data_without_labels, sigma_value) {
  id_mat <- data.matrix(data_without_labels, rownames.force = NA)
  rotate <- function(x) t(apply(x, 2, rev))
  
  # Smooth all images
  for(i in 1:nrow(id_mat)) {
    rotated <- c(id_mat[i,2:ncol(data_without_labels)])
    image <- matrix(rotated,nrow = 18,ncol = 18, byrow = FALSE)
    image <- smoothImage(image, sigma_value)
    id_mat[i,2:ncol(id_mat)] <- matrix(image,nrow = 1,ncol = ncol(id_mat) - 1, byrow = FALSE)
  }
  idSmoothed <- as.data.frame(id_mat)
  #idSmoothed[,1] <- factor(id[,1]) #idSmoothed holds the smoothed image data
  return(idSmoothed)
}
gu_si_025 <- gaussianSmoothing(id[,-1], 0.25)
plotData2 (gu_si_025)
 
gu_si_05 <- gaussianSmoothing(id[,-1], 0.5)
plotData2 (gu_si_05)

gu_si_075 <- gaussianSmoothing(id[,-1], 0.75)
plotData2 (gu_si_075)

#######
## Principal Component Analysis (PCA)
#######
performPCA <- function(dataset){
  pca <- prcomp(dataset,scale=FALSE)
  return (pca)
}

get_prop_and_cum <- function(dataset) {
  raw_id_pca <- performPCA(dataset)
  
  eigs <- raw_id_pca$sdev^2
  Proportion = eigs/sum(eigs)
  Cumulative = cumsum(eigs)/sum(eigs)
  return(list(Proportion, Cumulative))
}
raw_pca_prop_and_cum <- get_prop_and_cum(id[,-1])

plot(raw_pca_prop_and_cum[[1]], type="o", xlab="Principal Component", ylab="Proportion of variance")
plot(raw_pca_prop_and_cum[[2]], type="o", xlab="Principal Component", ylab="Cumulative sum of variance")

##
# Plot eigens
##
plotEigen <- function (pca_data) {
  id_mat <-data.matrix(pca_data$rotation[], rownames.force = NA)
  img <- matrix(,180,180)
  cipher <- 1
  for (x in 1:10){
    for (y in 1:10) {
      rotated <-c(id_mat[,(((x - 1) * 10) + y)])
      rotated <-((rotated -min(rotated)) / (max(rotated) -min(rotated)))
      
      tmpM <-matrix(rotated,18,18)
      
      for (xM in 1:18) {
        for (yM in 1:18) {
          img[(x-1)*18+xM, (y-1)*18+yM] <- tmpM[xM,yM]
        }
      }
    }
  }
  rotate <- function(x) t(apply(x, 2, rev))
  image(rotate(img),col=gray(0:100/100) )
}
raw_id_pca <- performPCA(id[,-1])
plotEigen(raw_id_pca)


##
# Combining
##

id_min_max_normalized <- as.data.frame(lapply(id[-1], normalize))
id_z_normalized <- as.data.frame(scale(id[-1]))
id_gau <- gaussianSmoothing(id[-1], 0.75)

#MIN MAX
min_max_gau <- gaussianSmoothing(id_min_max_normalized, 0.75)
plotData2 (min_max_gau)

min_max_gau_z <- as.data.frame(scale(min_max_gau))
plotData2 (min_max_gau_z)

min_max_z <- as.data.frame(scale(id_min_max_normalized))
plotData2 (min_max_z)

min_max_z_gau <- gaussianSmoothing(min_max_z, 0.75)
plotData2 (min_max_z_gau)


# GAU
gau_min_max <- as.data.frame(lapply(id_gau, normalize))
plotData2 (gau_min_max)

gau_min_max_z <- as.data.frame(scale(gau_min_max))
plotData2 (gau_min_max_z)

gau_z <- as.data.frame(scale(id_gau))
plotData2 (gau_z)

gau_z_min_max <- as.data.frame(lapply(gau_z, normalize))
plotData2 (gau_z_min_max)

#Z norm
z_min_max <- as.data.frame(lapply(id_z_normalized, normalize))
plotData2 (z_min_max)

z_min_max_gau <- gaussianSmoothing(z_min_max, 0.75)
plotData2 (z_min_max_gau)

z_gau <- gaussianSmoothing(id_z_normalized, 0.75)
plotData2 (z_gau)

z_gau_min_max <- as.data.frame(lapply(z_gau, normalize))
plotData2 (z_gau_min_max)

#PCA
get_prop_and_cum_from_pca <- function(data_pca) {
  eigs <- data_pca$sdev^2
  Proportion = eigs/sum(eigs)
  Cumulative = cumsum(eigs)/sum(eigs)
  return(list(Proportion, Cumulative))
}
#MIN MAX
min_max_pca <- performPCA(id_min_max_normalized)
min_max_pca_prop_cum <- get_prop_and_cum_from_pca(min_max_pca)
min_max_gau_pca <- performPCA(min_max_gau)
min_max_gau_pca_prop_cum <- get_prop_and_cum_from_pca(min_max_gau_pca)
min_max_gau_z_pca <- performPCA(min_max_gau_z)
min_max_gau_z_pca_prop_cum <- get_prop_and_cum_from_pca(min_max_gau_z_pca)
min_max_z_pca <- performPCA(min_max_z)
min_max_z_pca_prop_cum <- get_prop_and_cum_from_pca(min_max_z_pca)
min_max_z_gau_pca <- performPCA(min_max_z_gau)
min_max_z_gau_pca_prop_cum <- get_prop_and_cum_from_pca(min_max_z_gau_pca)

plot(min_max_pca_prop_cum[[1]], type="b", col=1, lwd=1, pch=1, xlab="Principal Component", ylab="Proportion of variance", xlim=range(0,10), ylim=range(0,0.5))
plot_labels <- c("min_max_pca")
lines(min_max_gau_pca_prop_cum[[1]], type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- c("min_max_gau_pca")
lines(min_max_gau_z_pca_prop_cum[[1]], type="b", col=3, lwd=1, pch=3)
plot_labels[3] <- c("min_max_gau_z_pca")
lines(min_max_z_pca_prop_cum[[1]], type="b", col=4, lwd=1, pch=4)
plot_labels[4] <- c("min_max_z_pca")
lines(min_max_z_gau_pca_prop_cum[[1]], type="b", col=5, lwd=1, pch=5)
plot_labels[5] <- c("min_max_z_gau_pca")
lines(raw_pca_prop_and_cum[[1]], type="b", col=6, lwd=1, pch=6)
plot_labels[6] <- c("raw_data_pca")
legend("right",plot_labels, lwd=c(1), col=c(1:6), pch=c(1:6), y.intersp=1)

plot(min_max_pca_prop_cum[[2]], type="b", col=1, lwd=1, pch=1, xlab="Principal Component", ylab="Cumulative sum of variance", xlim=range(0,75))
plot_labels <- c("min_max_pca")
lines(min_max_gau_pca_prop_cum[[2]], type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- c("min_max_gau_pca")
lines(min_max_gau_z_pca_prop_cum[[2]], type="b", col=3, lwd=1, pch=3)
plot_labels[3] <- c("min_max_gau_z_pca")
lines(min_max_z_pca_prop_cum[[2]], type="b", col=4, lwd=1, pch=4)
plot_labels[4] <- c("min_max_z_pca")
lines(min_max_z_gau_pca_prop_cum[[2]], type="b", col=5, lwd=1, pch=5)
plot_labels[5] <- c("min_max_z_gau_pca")
lines(raw_pca_prop_and_cum[[2]], type="b", col=6, lwd=1, pch=6)
plot_labels[6] <- c("raw_data_pca")
legend("right",plot_labels, lwd=c(1), col=c(1:6), pch=c(1:6), y.intersp=1)

#Gau
gau_pca <- performPCA(id_gau)
gau_pca_prop_cum <- get_prop_and_cum_from_pca(gau_pca)
gau_min_max_pca <- performPCA(gau_min_max)
gau_min_max_pca_prop_cum <- get_prop_and_cum_from_pca(gau_min_max_pca)
gau_min_max_z_pca <- performPCA(gau_min_max_z)
gau_min_max_z_pca_prop_cum <- get_prop_and_cum_from_pca(gau_min_max_z_pca)
gau_z_pca <- performPCA(gau_z)
gau_z_pca_prop_cum <- get_prop_and_cum_from_pca(gau_z_pca)
gau_z_min_max_pca <- performPCA(gau_z_min_max)
gau_z_min_max_pca_prop_cum <- get_prop_and_cum_from_pca(gau_z_min_max_pca)

plot(gau_pca_prop_cum[[1]], type="b", col=1, lwd=1, pch=1, xlab="Principal Component", ylab="Proportion of variance", xlim=range(0,10), ylim=range(0,0.5))
plot_labels <- c("gau_pca")
lines(gau_min_max_pca_prop_cum[[1]], type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- c("gau_min_max_pca")
lines(gau_min_max_z_pca_prop_cum[[1]], type="b", col=3, lwd=1, pch=3)
plot_labels[3] <- c("gau_min_max_z_pca")
lines(gau_z_pca_prop_cum[[1]], type="b", col=4, lwd=1, pch=4)
plot_labels[4] <- c("gau_z_pca")
lines(gau_z_min_max_pca_prop_cum[[1]], type="b", col=5, lwd=1, pch=5)
plot_labels[5] <- c("gau_z_min_max_pca")
lines(raw_pca_prop_and_cum[[1]], type="b", col=6, lwd=1, pch=6)
plot_labels[6] <- c("raw_data_pca")
legend("right",plot_labels, lwd=c(1), col=c(1:6), pch=c(1:6), y.intersp=1)

plot(gau_pca_prop_cum[[2]], type="b", col=1, lwd=1, pch=1, xlab="Principal Component", ylab="Cumulative sum of variance", xlim=range(0,75))
plot_labels <- c("gau_pca")
lines(gau_min_max_pca_prop_cum[[2]], type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- c("gau_min_max_pca")
lines(gau_min_max_z_pca_prop_cum[[2]], type="b", col=3, lwd=1, pch=3)
plot_labels[3] <- c("gau_min_max_z_pca")
lines(gau_z_pca_prop_cum[[2]], type="b", col=4, lwd=1, pch=4)
plot_labels[4] <- c("gau_z_pca")
lines(gau_z_min_max_pca_prop_cum[[2]], type="b", col=5, lwd=1, pch=5)
plot_labels[5] <- c("gau_z_min_max_pca")
lines(raw_pca_prop_and_cum[[2]], type="b", col=6, lwd=1, pch=6)
plot_labels[6] <- c("raw_data_pca")
legend("right",plot_labels, lwd=c(1), col=c(1:6), pch=c(1:6), y.intersp=1)

#Z
z_pca <- performPCA(id_z_normalized)
z_pca_prop_cum <- get_prop_and_cum_from_pca(z_pca)
z_min_max_pca <- performPCA(z_min_max)
z_min_max_pca_prop_cum <- get_prop_and_cum_from_pca(z_min_max_pca)
z_min_max_gau_pca <- performPCA(z_min_max_gau)
z_min_max_gau_pca_prop_cum <- get_prop_and_cum_from_pca(z_min_max_gau_pca)
z_gau_pca <- performPCA(z_gau)
z_gau_pca_prop_cum <- get_prop_and_cum_from_pca(z_gau_pca)
z_gau_min_max_pca <- performPCA(z_gau_min_max)
z_gau_min_max_pca_prop_cum <- get_prop_and_cum_from_pca(z_gau_min_max_pca)

plot(z_pca_prop_cum[[1]], type="b", col=1, lwd=1, pch=1, xlab="Principal Component", ylab="Proportion of variance", xlim=range(0,10), ylim=range(0,0.5))
plot_labels <- c("z_pca")
lines(z_min_max_pca_prop_cum[[1]], type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- c("z_min_max_pca")
lines(z_min_max_gau_pca_prop_cum[[1]], type="b", col=3, lwd=1, pch=3)
plot_labels[3] <- c("z_min_max_gau_pca")
lines(z_gau_pca_prop_cum[[1]], type="b", col=4, lwd=1, pch=4)
plot_labels[4] <- c("z_gau_pca")
lines(z_gau_min_max_pca_prop_cum[[1]], type="b", col=5, lwd=1, pch=5)
plot_labels[5] <- c("z_gau_min_max_pca")
lines(raw_pca_prop_and_cum[[1]], type="b", col=6, lwd=1, pch=6)
plot_labels[6] <- c("raw_data_pca")
legend("right",plot_labels, lwd=c(1), col=c(1:6), pch=c(1:6), y.intersp=1)

plot(z_pca_prop_cum[[2]], type="b", col=1, lwd=1, pch=1, xlab="Principal Component", ylab="Cumulative sum of variance", xlim=range(0,75))
plot_labels <- c("z_pca")
lines(z_min_max_pca_prop_cum[[2]], type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- c("z_min_max_pca")
lines(z_min_max_gau_pca_prop_cum[[2]], type="b", col=3, lwd=1, pch=3)
plot_labels[3] <- c("z_min_max_gau_pca")
lines(z_gau_pca_prop_cum[[2]], type="b", col=4, lwd=1, pch=4)
plot_labels[4] <- c("z_gau_pca")
lines(z_gau_min_max_pca_prop_cum[[2]], type="b", col=5, lwd=1, pch=5)
plot_labels[5] <- c("z_gau_min_max_pca")
lines(raw_pca_prop_and_cum[[2]], type="b", col=6, lwd=1, pch=6)
plot_labels[6] <- c("raw_data_pca")
legend("right",plot_labels, lwd=c(1), col=c(1:6), pch=c(1:6), y.intersp=1)

plotEigen(min_max_gau_pca)

###########
## Random forests 
## Create a Random Forest classifier and evaluate it using cross validation.
###########
# Resources
# https://www.blopig.com/blog/2017/04/a-very-basic-introduction-to-random-forests-using-r/
# https://rpubs.com/Gonzo25/94920
# https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/rfcv

# Logging to get to know random forest
#model
# Using the importance() function, we can view the importance of each variable.
#importance(model)

#######
## Find the best amount of PC's to use
#######
performPCA <- function(dataset, pcaCount){
  pca <- prcomp(dataset,scale=FALSE, rank. = pcaCount)
  return (pca)
}

pca <- prcomp(id,scale=FALSE)
summary(pca)

calculateExplainedVariance <- function(pca){
  #The variance explained by each principal component is obtained by squaring these:
  pr.var=pca$sdev^2 
  return (pr.var)
}
pr.var <- calculateExplainedVariance(pca)
# To compute the proportion of variance explained by each principal component, 
# we simply divide the variance explained by each principal component by the total variance explained by all four principal components:
calculatePropOfVariance <- function(pr.var){
  pve=pr.var/sum(pr.var)
  return (pve)
}

pve <- calculatePropOfVariance(pr.var)
pve

# A plot of the variance
plot(pr.var[1:300], xlab="Principal Component", ylab="Variance", ylim=c(0,40),type="b")
abline(v = 8, col="green", lty=5)
abline(v = 67, col="red", lty=5)

#A plot of the PVE explained by each component
plot(pve[1:10], xlab="Principal Component", ylab="Proportion of Variance Explained",type="b")
abline(v = 8, col="red", lty=5)

#the cumulative PVE plotted. This shows the CDF and tells us when x% of variance is explained. Only take the first 100 PCA's
plot(cumsum(pve[1:100]), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type="b")

#Transform data
transform.data <- function(dataset, pca){
  transformed.dataset <- predict(pca, newdata = dataset)
  # Convert to transformed dataset to dataframe
  df.transformed.dataset <- as.data.frame(transformed.dataset) 
  return (df.transformed.dataset)
}
###
#Cross validation for evaluating random forest
##

cross_validation_random_forest <- function(data_set, seed, numberoftrees) {
  set.seed(seed)
  folds <-createFolds(data_set[, 1], k = 10) #data_set$X1
  
  tree.count <- numberoftrees
  
  #model <- randomForest(States ~ PC1 + PC2 + PC3 + PC4 + PC5, data = data_set, ntree=tree.count)
  
  cross_validation_results <- lapply(folds, function(x) {
    
    #90% of the entire dataset is assigned to data_train
    data_train_with_labels <- data_set[-x, ]
    data_train <- data_train_with_labels[, -1]
    data_train_labels <- data_train_with_labels[ , 1]
    
    # The rest 10% of the dataset is assigned to data_test
    data_test_with_labels <- data_set[x, ]
    data_test <- data_test_with_labels[, -1]
    data_test_labels <- data_test_with_labels[ , 1]
    
    #Perform PCA but with only with 5 principal components
    pca.5 <- performPCA(data_train, 5)
    
    #Take one person and transform dataset and add States column
    df.transformed.dataset <- transform.data(data_train_with_labels, pca.5)
    datanew <- cbind(df.transformed.dataset, data_train_with_labels[,1]) # 'df.transformed.dataset' is a dataframe
    datanew$States <- factor(datanew[,6])
    
    start_time <- Sys.time()
    # Create Random Forest classifer. Uses single person.
    model <- randomForest(States ~ PC1 + PC2 + PC3 + PC4 + PC5, data = datanew, ntree=tree.count)
    end_time <- Sys.time()
    
    df.transformed.test_dataset <- transform.data(data_test_with_labels, pca.5)
    datanew_test <- cbind(df.transformed.test_dataset, data_train_with_labels[,1]) # 'df.transformed.dataset' is a dataframe
    datanew_test$States <- factor(datanew_test[,6])
    
    #Predict random vs. test set
    data_pred <-predict(model,datanew_test)
    
    run_time <- end_time - start_time
    
    accuracy = mean(data_pred == data_test_labels)
    
    cat("Accuracy:", accuracy, sep = "\n")
    
    return (c(accuracy, run_time))
  })
  return(cross_validation_results)
}

res_cross_validation_with_random_forest.100 <- cross_validation_random_forest(dataset_shuffle,123, 10)
res_cross_validation_with_random_forest.500 <- cross_validation_random_forest(dataset_shuffle,123, 500)

res_random_forest.100 <- do.call(rbind, res_cross_validation_with_random_forest.100[])
res_random_forest.100 <- as.data.frame(res_random_forest.100)

res_random_forest.500 <- do.call(rbind, res_cross_validation_with_random_forest.500[])
res_random_forest.500 <- as.data.frame(res_random_forest.500)

#Plot only 100
boxplot(res_random_forest.100[[1]], names=c("100 trees"), main="Accuracy")
boxplot(res_random_forest.100[[2]], names=c("100 trees"), main="Run time in s")

#Plot both 100 and 500 
boxplot(res_random_forest.100[[1]],res_random_forest.500[[1]], names=c("100 trees","500 trees"), main="Accuracy")
boxplot(res_random_forest.100[[2]],res_random_forest.500[[2]], names=c("100 trees","500 trees"), main="Run time in s")



###########################################################################
########################### K N N section #################################
###########################################################################




###########
## Cross-validate KNN, 10-folds
###########
cross_validation_knn <- function(data_set, seed, k_value) {
  set.seed(seed)
  
  folds <-createFolds(data_set[, 1], k = 10) 
  
  accuracyList <- c(1:10)
  runtimeList <- c(1:10)

  for(i in 1:10)
  {
    id_train <- data_set[-folds[[i]],-1]
    id_test <- data_set[folds[[i]],-1]
    
    id_train_labels <- data_set[-folds[[i]],1]
    id_test_labels <- data_set[folds[[i]],1]
    
    start_time <- Sys.time()
    id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=k_value)
    run_time <- difftime(Sys.time(), start_time, units = "secs")
    
    cf <- confusionMatrix(id_test_labels, id_test_pred)
    accuracyList[i] <- sum(diag(cf$table))/sum(cf$table)
    runtimeList[i] <- run_time

    
  }
  
  meanRuntime <- mean(runtimeList)
  varianceRuntime <- var(runtimeList)
  meanAccuracy <- mean(accuracyList)
  varianceAccuracy <- var(accuracyList)
  
  return(c(meanAccuracy, varianceAccuracy, meanRuntime, varianceRuntime))
    
}
###########
## Cross-validate KNN with PCA, 10-folds
###########
cross_validation_knn_pca <- function(data_set, seed, k_value, pca_count) {
  set.seed(seed)
  
  folds <-createFolds(data_set[, 1], k = 10) 
  
  accuracyList <- c(1:10)
  runtimeList <- c(1:10)
  
  for(i in 1:10) {
    id_train <- data_set[-folds[[i]],-1]
    id_test <- data_set[folds[[i]],-1]
    
    id_train_labels <- data_set[-folds[[i]],1]
    id_test_labels <- data_set[folds[[i]],1]
    
    # Zhuoqi's code
    id_pca <- prcomp(id_train, center = TRUE, scale. = TRUE, rank. = pca_count)
    
    test_pca <- predict(id_pca,id_test)
    
    id_train <- id_pca$x
    id_test <- test_pca
    # End 
    
    start_time <- Sys.time()
    id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=k_value)
    run_time <- difftime(Sys.time(), start_time, units = "secs")
    
    cf <- confusionMatrix(id_test_labels, id_test_pred)
    accuracyList[i] <- sum(diag(cf$table))/sum(cf$table)
    runtimeList[i] <- run_time
    
    
  }
  
  meanRuntime <- mean(runtimeList)
  varianceRuntime <- var(runtimeList)
  meanAccuracy <- mean(accuracyList)
  varianceAccuracy <- var(accuracyList)
  
  return(c(meanAccuracy, varianceAccuracy, meanRuntime, varianceRuntime))
}


###########
## Creates 2 lists containing standard deviation from mean, based on variance
###########
std_dev_lists <- function(variance_list_input, mean_list_input) { 
  
  upper <- c()
  lower <- c()
  
  variance_list <- unlist(variance_list_input)
  mean_list <- unlist(mean_list_input)
  length <- length(mean_list)
  
  for(i in 1:length) { 
    variance <- variance_list[[i]][1]
    mean <- mean_list[[i]][1]
    std_dev <- sqrt(variance)
    upper[i] <- mean + std_dev
    lower[i] <- mean - std_dev
    
  }
  
  result <- list(upper, lower)
  return(result)
}


############################################
# Plots results of CV on KNN for 10 k values
############################################
knn_cv_and_plot <- function(dataset, description) { 
  
  accuracyList <- c()
  runtimeList <- c()
  varianceList <- c()
  resultList <- c()
  for (i in seq(1,101, by = 10)) { 
    resultList[[i]] <- cross_validation_knn(dataset, 1234, i)
    accuracyList[[i]] <- resultList[[i]][1]
    varianceList[[i]] <- resultList[[i]][2]
    runtimeList[[i]] <- resultList[[i]][3]
  } 
  
  std_dev <- std_dev_lists(varianceList, accuracyList)
  upper <- std_dev[[1]]
  lower <- std_dev[[2]]
  
  plot(c(1,11,21,31,41,51,61,71,81,91,101), unlist(accuracyList), type="b", col=3, lwd=3, pch=1, xlab="K value", ylab="Mean Accuracy [%]")
  plot_labels <- c("Mean Accuracy")
  lines(c(1,11,21,31,41,51,61,71,81,91,101), unlist(upper), type="b", col=2, lwd=1, pch=2)
  plot_labels[2] <- paste("Std. Deviation")
  lines(c(1,11,21,31,41,51,61,71,81,91,101), unlist(lower), type="b", col=2, lwd=1, pch=2)
  plot_labels[3] <- paste("Std. Deviation")
  polygon(c(c(1,11,21,31,41,51,61,71,81,91,101), rev(c(1,11,21,31,41,51,61,71,81,91,101))), c(upper, rev(lower)), col = adjustcolor("red",alpha.f=0.2) )
  legend("bottomleft",plot_labels, lwd=c(1), col=c(3,2,2), pch=c(1,2,2), y.intersp=1)
  header <- paste("Accuracy, ",description, sep="")
  title(header)
  
  plot(c(1,11,21,31,41,51,61,71,81,91,101), unlist(runtimeList), type="b", col=1, lwd=1, pch=1, xlab="K value", ylab="Mean Runtime [s]")
  header <- paste("Runtime, ",description, sep="")
  title(header)
  
}

##################################################
# Plots results of CV on KNN for 3 k values
##################################################
knn_cv_and_plot_small_run <- function(dataset, description, pca_count) { 
  
  accuracyList <- c()
  runtimeList <- c()
  varianceList <- c()
  resultList <- c()
  for (i in seq(1,101, by = 50)) { 
    resultList[[i]] <- cross_validation_knn(dataset, 1234, i, pca_count)
    accuracyList[[i]] <- resultList[[i]][1]
    varianceList[[i]] <- resultList[[i]][2]
    runtimeList[[i]] <- resultList[[i]][3]
  } 
  
  std_dev <- std_dev_lists(varianceList, accuracyList)
  upper <- std_dev[[1]]
  lower <- std_dev[[2]]
  
  plot(c(1,51,101), unlist(accuracyList), type="b", col=3, lwd=3, pch=1, xlab="K value", ylab="Mean Accuracy [%]")
  plot_labels <- c("Mean Accuracy")
  lines(c(1,51,101), unlist(upper), type="b", col=2, lwd=1, pch=2)
  plot_labels[2] <- paste("Std. Deviation")
  lines(c(1,51,101), unlist(lower), type="b", col=2, lwd=1, pch=2)
  plot_labels[3] <- paste("Std. Deviation")
  polygon(c(c(1,51,101), rev(c(1,51,101))), c(upper, rev(lower)), col = adjustcolor("red",alpha.f=0.2) )
  legend("bottomleft",plot_labels, lwd=c(1), col=c(3,2,2), pch=c(1,2,2), y.intersp=1)
  header <- paste("Accuracy, ",description, sep="")
  title(header)
  
  plot(c(1,51,101), unlist(runtimeList), type="b", col=1, lwd=1, pch=1, xlab="K value", ylab="Mean Runtime [s]")
  header <- paste("Runtime, ",description, sep="")
  title(header)
}


##################################################
# Plots results of CV on KNN w/ PCA for 3 k values
##################################################
knn_cv_and_plot_pca <- function(dataset, description, pca_count) { 
  
  accuracyList <- c()
  runtimeList <- c()
  varianceList <- c()
  resultList <- c()
  for (i in seq(1,101, by = 50)) { 
    resultList[[i]] <- cross_validation_knn_pca(dataset, 1234, i, pca_count)
    accuracyList[[i]] <- resultList[[i]][1]
    varianceList[[i]] <- resultList[[i]][2]
    runtimeList[[i]] <- resultList[[i]][3]
  } 
  
  std_dev <- std_dev_lists(varianceList, accuracyList)
  upper <- std_dev[[1]]
  lower <- std_dev[[2]]
  
  plot(c(1,51,101), unlist(accuracyList), type="b", col=3, lwd=3, pch=1, xlab="K value", ylab="Mean Accuracy [%]")
  plot_labels <- c("Mean Accuracy")
  lines(c(1,51,101), unlist(upper), type="b", col=2, lwd=1, pch=2)
  plot_labels[2] <- paste("Std. Deviation")
  lines(c(1,51,101), unlist(lower), type="b", col=2, lwd=1, pch=2)
  plot_labels[3] <- paste("Std. Deviation")
  polygon(c(c(1,51,101), rev(c(1,51,101))), c(upper, rev(lower)), col = adjustcolor("red",alpha.f=0.2) )
  legend("bottomleft",plot_labels, lwd=c(1), col=c(3,2,2), pch=c(1,2,2), y.intersp=1)
  header <- paste("Accuracy, ",description, sep="")
  title(header)
  
  plot(c(1,51,101), unlist(runtimeList), type="b", col=1, lwd=1, pch=1, xlab="K value", ylab="Mean Runtime [s]")
  header <- paste("Runtime, ",description, sep="")
  title(header)
}


#Raw, all persons in, 10 different k vals
knn_cv_and_plot(dataset_shuffle, "raw data - all persons in") #### WARNING: Roughly 11 hour runtime

#Raw, disjunct, 10 different k vals
knn_cv_and_plot(id, "raw data - disjunct") #### WARNING: Roughly 11 hour runtime





#PCA on raw, all persons in data
knn_cv_and_plot_pca(dataset_shuffle, "raw data - all persons in, 10 PCs", 10)
knn_cv_and_plot_pca(dataset_shuffle, "raw data - all persons in, 21 PCs", 21)
knn_cv_and_plot_pca(dataset_shuffle, "raw data - all persons in, 31 PCs", 31)


#PCA on raw, disjunct data
knn_cv_and_plot_pca(id, "raw data - disjunct, 10 PCs", 10)
knn_cv_and_plot_pca(id, "raw data - disjunct, 21 PCs", 21)
knn_cv_and_plot_pca(id, "raw data - disjunct, 31 PCs", 31)



library(rpart)
library(rpart.plot) #SKAL INSTALLERES
library(stats)
library(randomForest)
library(caret)
library(kernlab)
library(RSNNS)

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
      tmpM <- matrix(d[(x*200-idx),2:325],18,18)
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
#######
## Min max normalization
#######
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

id_min_max_normalized <- as.data.frame(lapply(id[-1], normalize))
id_min_max_normalized <- cbind(V1 = 0, id_min_max_normalized)

min_max_disjunct_train <- id_min_max_normalized[1:(items_per_person*30),]
min_max_disjunct_test <- id_min_max_normalized[(items_per_person*30 + 1):(items_per_person*38),]

set.seed(1234)
dataset_shuffle <-id_min_max_normalized[sample(nrow(id_min_max_normalized)),]
min_max_all_persons_train <- dataset_shuffle[1:(items_per_person*30),]
min_max_all_persons_test <- dataset_shuffle[(items_per_person*30 + 1):(items_per_person*38),]

#######
## Z normalization
#######
id_z_normalized <- as.data.frame(scale(id[-1]))
id_z_normalized <- cbind(V1 = 0, id_z_normalized)

z_normalized_disjunct_train <- id_z_normalized[1:(items_per_person*30),]
z_normalized_disjunct_test <- id_z_normalized[(items_per_person*30 + 1):(items_per_person*38),]

set.seed(1234)
dataset_shuffle <-id_z_normalized[sample(nrow(id_z_normalized)),]
z_normalized_all_persons_train <- dataset_shuffle[1:(items_per_person*30),]
z_normalized_all_persons_test <- dataset_shuffle[(items_per_person*30 + 1):(items_per_person*38),]

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















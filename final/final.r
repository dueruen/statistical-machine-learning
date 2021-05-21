library(rpart)
library(rpart.plot) #SKAL INSTALLERES
library(stats)
library(randomForest)
library(caret)
library(kernlab)
library(RSNNS)
library(gmodels)
library(class)
library(caret)
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
id_min_max_normalized <- cbind(V1 = id[1], id_min_max_normalized)

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
id_z_normalized <- cbind(V1 = id[1], id_z_normalized)

z_normalized_disjunct_train <- id_z_normalized[1:(items_per_person*30),]
z_normalized_disjunct_test <- id_z_normalized[(items_per_person*30 + 1):(items_per_person*38),]

set.seed(1234)
dataset_shuffle <-id_z_normalized[sample(nrow(id_z_normalized)),]
z_normalized_all_persons_train <- dataset_shuffle[1:(items_per_person*30),]
z_normalized_all_persons_test <- dataset_shuffle[(items_per_person*30 + 1):(items_per_person*38),]


cross_validation <- function(data_set, seed, k_value) {
  set.seed(seed)

  folds <-createFolds(data_set[, 1], k = 10) 
  
  #lapply() returns a list of the same length as X, each element of which is the result 
  # of applying FUN to the corresponding element of X
  cross_validation_results <- lapply(folds, function(x) {
    
    
    # Training data
    data_train_with_labels <- data_set[-x, ]
    df.train.data <- data_train_with_labels[, -1]
    df.train.labels <- data_train_with_labels[ , 1]
    
    # Test data
    data_test_with_labels <- data_set[x, ]
    df.test.data <- data_test_with_labels[, -1]
    data_test_labels <- data_test_with_labels[ , 1]
    
    
    
    # KNN on the data set
    start_time <- Sys.time()
    knn_classifier <- knn(train = df.train.data, test = df.test.data, cl = df.train.labels, k = 101)
    run_time <- difftime(Sys.time(), start_time, units = "secs")
    
    # Calc the accuracy of knn
    accuracy.test = mean(knn_classifier == data_test_labels)
    accuracy.training = mean(knn_classifier == df.train.labels)
    
    
    ## How to get variance?
    
    # Calc the error rate of knn
    error.rate.test = mean(knn_classifier != data_test_labels)
    error.rate.training = mean(knn_classifier != df.train.labels)
    return(list(accuracy.training, error.rate.training, accuracy.test, error.rate.test, run_time))
  })
  return(cross_validation_results)
}

knn_cv_plot <- function(data_set) { 

  
}

resultList <- c()

#for (i in seq(1, 101, by = 10)) {
  
  result <- cross_validation(dataset_shuffle, 101)
  
#}















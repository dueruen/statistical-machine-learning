#Packages
library(gmodels)
library(class)
library(caret)
library(e1071)
library(ggplot2)

#Load in data
load("id100.Rda")
load("idList-co-100.Rdata")

knn_and_cross <- function(data_set, seed){
  #Start seeding the data
  set.seed(seed)

  #Shuffle the dataset id100.Rda called data_set
  dataset_shuffle <-data_set[sample(nrow(data_set)),]  
  
  # Remove first column and assign to dataset_shuffle_no_labels
  dataset_shuffle_no_labels <-dataset_shuffle[ , -1]
  
  # IF THE dataset had column ID - Remove the column ID Regardless of the machine learning method, ID variables
  # should always be excluded. Neglecting to do so can lead to
  # erroneous findings because the ID can be used to uniquely
  # "predict" each example
  
  #Let's drop the id feature altogether. As it is located in the first column, we can
  # exclude it by making a copy of the wbcd data frame without column 1:
  #dataset_shuffle <- id[-1]
  
  # Split the dataset 50/50 for training and for testing. Remember that data is extracted from data frames
  # using the [row, column] syntax. A blank value for the row or column value
  # indicates that all rows or columns should be included
  # dataset_training <- dataset_shuffle[1:2000,]
  # dataset_test <- dataset_shuffle[2001:4000,]
  dataset_training <- dataset_shuffle_no_labels[1:(nrow(dataset_shuffle_no_labels) / 2), ]
  dataset_test <- dataset_shuffle_no_labels[(((nrow(dataset_shuffle_no_labels) / 2)) + 1):(nrow(dataset_shuffle_no_labels)), ]
  
  # For training the kNN model, we will need to store these class labels in
  # factor vectors, divided to the training and test datasets:
  #dataset_training_labels <- dataset_shuffle[1:2000, 1]
  #dataset_test_labels <- dataset_shuffle[2001:4000, 1]
  dataset_training_labels <- dataset_shuffle[1:(nrow(dataset_shuffle) / 2), 1]
  dataset_test_labels <- dataset_shuffle[(((nrow(dataset_shuffle) / 2)) + 1):(nrow(dataset_shuffle)), 1]
  
  #This code takes the diagnosis factor in column 1 of the wbcd data frame and creates
  # the vectors, wbcd_train_labels and wbcd_test_labels. We will use these in the
  # next steps of training and evaluating our classifier.
  
  # knn() can be used for training and classification and needs 4 parameters(train, test, class, k) sqrt(størrelse af dataset)
  # The function returns a factor vector of predicted classes for each row in the test data frame
  start_time <- Sys.time()
  dataset_predicted <- knn(train = dataset_training, test = dataset_test, cl = dataset_training_labels, k = 63)
  end_time <- Sys.time()
  
  #http://www.socr.umich.edu/people/dinov/courses/DSPA_notes/06_LazyLearning_kNN.html good link for the example
  # Create a cross tabulation indicating the agreement between the two vectors.
  # Specifying prop.chisq = FALSE will remove the chi-square values that are not needed, from the output:
  CrossTable(x = dataset_test_labels, y = dataset_predicted, prop.chisq = FALSE)
  #(x = dataset_test_labels, y = dataset_predicted, prop.chisq = FALSE)
  
  # Runtime of knn
  run_time <- end_time - start_time
  str(run_time)
  
  # 1.4.2 - Performance of varying K: Analyse performance with varying K.
  # Inspired by: http://www.socr.umich.edu/people/dinov/courses/DSPA_notes/06_LazyLearning_kNN.html
  #install.packages("e1071", dependencies = TRUE)
  knntuning = tune.knn(x= dataset_training, y = dataset_training_labels, k = seq(1, 101, by = 20))
  summary(knntuning)  
}

knn_and_cross(id, 423)

#1.4.3 Cross validation: Perform a cross validation with a 90% / 10% split with 10 runs. 
# Report mean and standard deviation of the performance
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

cross_validation_and_print <- function (data_set, seed) {
  cross_val_res <- cross_validation(data_set, seed)
  
  #Examine the results
  str(cross_val_res)
  
  #Calculate mean of these 10 results while cross_validation_results is not a numeric vector, 
  # i'll have to do unlist() which creates a numeric vector where i can use mean() 
  cat(" The mean kappa statistic is:", mean(unlist(cross_val_res)))
  cat(" The standard deviation for kappa is:", sd(unlist(cross_val_res)))
}

cross_validation_and_print(id, 123)


#1.4.4
idLoaded <- do.call(rbind, idList[1:10]) 
idLoaded <- as.data.frame(idLoaded) 
idLoaded[, 1] <- factor(idLoaded[, 1])

## WARNING ## TAKES A LONG TIME 1-5min
knn_and_cross(idLoaded, 423)
## WARNING ## TAKES A EVEN LONG TIME 10-30min
cross_validation(idLoaded, 123)

# Cal mean and sd for the individual students
res <- lapply(c(1:10), function(x) {
  l <- do.call(rbind, idList[x:x]) 
  l <- as.data.frame(l) 
  l[, 1] <- factor(l[, 1])
  cross_val_res <- cross_validation(l, 123)
  res_mean <- mean(unlist(cross_val_res))
  res_sd <- sd(unlist(cross_val_res))
  return(c(res_mean, res_sd))
})
str(res)


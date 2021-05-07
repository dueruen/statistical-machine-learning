library(rpart)
library(rpart.plot) #SKAL INSTALLERES
library(stats)
library(randomForest)
library(caret)
library(kernlab)
library(RSNNS)

#Load the dataset
load("idList-cornered-100-2021.Rdata")

id <- do.call(rbind, idList[])
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

items_per_person = nrow(id) / length(idList)
id_train <- id[1:(items_per_person*9),]
id_test <- id[(items_per_person*9 + 1):(items_per_person*13),]

#####################
## Exercise 5.1 - SVM
#####################

#########
## 5.1.1
#########

start_time <- Sys.time()
classifier_rbf <- ksvm(V1~ ., data = id_train, kernel = "vanilladot", kpar="automatic", C = 1)
end_time <- Sys.time()
run_time <- end_time - start_time

classifier_pre <- predict(classifier_rbf,id_test)

cf <- confusionMatrix(classifier_pre, id_test[,1])
print( sum(diag(cf))/sum(cf))
print( run_time )

#########
## 5.1.2
#########
cs <- c(0.1,0.5, 1,2,3,4,5)
c_results2 <- lapply(cs, function(x) {
  start_time_train <- Sys.time()
  classifier_rbf <- ksvm(V1~ ., data = id_train, kernel = "rbfdot", kpar="automatic", C = x)
  end_time_train <- Sys.time()
  run_time_train <- end_time_train - start_time_train
  
  classifier_pre <- predict(classifier_rbf,id_test)
  
  cf <- confusionMatrix(classifier_pre, id_test[,1])
  acc <- sum(diag(cf))/sum(cf)
  cat("train done")

  classifier_pre_with_train <- predict(classifier_rbf,id_train)
  
  cf_with_train <- confusionMatrix(classifier_pre_with_train, id_train[,1])
  acc_with_train <- sum(diag(cf_with_train))/sum(cf_with_train)
  cat("test done")
  
  return (c(acc, run_time_train, acc_with_train))
})

res <- do.call(rbind, c_results[])
res <- as.data.frame(res)

plot(cs, res[[1]], type="b", col=1, lwd=1, pch=1, xlab="C value", ylab="Accuracy",ylim=range(0.645,1))
plot_labels <- c("test data")
lines(cs, res[[3]], type="b", col=3, lwd=1, pch=3)
plot_labels[2] <- c("training data")
title("Accuracy with different C values")
legend("right",plot_labels, lwd=c(1), col=c(1,3), pch=c(1,3), y.intersp=1)

plot(cs, res[[2]], type="b", col=1, lwd=1, pch=1, xlab="C value", ylab="Run time in minutes")
title("Runtime in minutes with different C values")


####################################
####### 5.2: Neural Networks ####### 
####################################

#######################################################################################################
## 5.2.1
## Create a matrix (as below in grey background) for the training classes. It has N rows (the same as 
## the number of training data) and 10 columns (binary). The column with '1' marks the 
## corresponding class as the example shown below (eg. cyper '0' is represented by '1000000000').
#######################################################################################################

lev <- levels(id_train$V1) # Number of classes

# Create a list probabilities, for all labels
nnTrainingClass <- matrix(nrow = length(id_train$V1), ncol = 10, data = 0) 

for(i in 1:length(id_train$V1)) { # Set probabilities to one for matching class
  matchList <- match(lev,toString(id_train$V1[i]))
  matchList[is.na(matchList)] <- 0
  nnTrainingClass[i,] <- matchList
}
trainingClass <- as.data.frame(nnTrainingClass)


############################################################################################
## 5.2.2
## Train a neural network with N inputs and 10 outputs, based on the modified training data.
############################################################################################


train_mlp <- function(training_data, training_classes, size) {

  network <- mlp(id_train[,-1], training_classes, size = size, maxit = 200, hiddenActFunc = "Act_TanH", 
                 learnFunc="Std_Backpropagation", learnFuncParams = c(0.01,0))
  
  plotIterativeError(network)
  
  network$IterativeFitError[200]
  
  return(network)
}

#network <- train_mlp(id_train, trainingClass, c(12, 3))


############################################################################################
## 5.2.3
## Evaluate the neural network with the test data.
############################################################################################
evaluate_nn <- function(network, test_data) { 

  
  predictions <- predict(network, test_data[,-1])
  #You can use the following code to convert the mlp output into class labels (0 - 9)
  responselist <- matrix(nrow = length(predictions[,1]), ncol = 1, data = "Na")
  for(i in 1:nrow(predictions)) {
    responselist[i,] <- toString( which(predictions[i,]==max(predictions[i,])) - 1 )
  }
  responselist <- data.frame(responselist)
  responselist[,1] <- as.factor(responselist[,1])
  # Calculating the accuracy
  cf <- confusionMatrix(responselist[,1], test_data[,1])
  return( sum(diag(cf))/sum(cf) )

}

#acc_train <- evaluate_nn(network, id_train)
#print("Accuracy when evaluated with training data:")
#print(acc_train)
#acc_test <- evaluate_nn(network, id_test)
#print("Accuracy when evaluated with testing data:")
#print(acc_test)

accuracyTestList <- c()
accuracyTrainingList <- c()
runtimeList <- c()
for (i in seq(1, 20, by = 2)) {
  start_time <- Sys.time()
  network <- train_mlp(id_train, trainingClass, (i+1))
  end_time <- Sys.time()
  runtimeList[[i+1]] <- end_time - start_time
  accuracyTestList[[i+1]] <- evaluate_nn(network, id_test)
  accuracyTrainingList[[i+1]] <- evaluate_nn(network, id_train)
  
}

plot(c(2,4,6,8,10,12,14,16,18,20), unlist(accuracyTrainingList), type="b", col=1, lwd=1, pch=1, xlab="Neurons in hidden layer", ylab="Accuracy")
plot_labels <- c("Training Data")
lines(c(2,4,6,8,10,12,14,16,18,20), unlist(accuracyTestList), type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- paste("Test Data")
#lines(c(2,4,6,8,10,12,14,16,18,20), unlist(runtimeList), type="b", col=3, lwd=1, pch=3)
#plot_labels[3] <- paste("Runtime of training")

title("NN Accuracy, 1 hidden layer, X neurons")
legend("bottomright",plot_labels, lwd=c(1), col=c(2,4,6,8,10,12,14,16,18,20), pch=c(2,4,6,8,10,12,14,16,18,20), y.intersp=1)


plot(c(2,4,6,8,10,12,14,16,18,20), unlist(runtimeList), type="b", col=1, lwd=1, pch=1, xlab="Neurons in hidden layer", ylab="Runtime in Minutes")
title("NN Training Runtime, 1 hidden layer, X neurons")

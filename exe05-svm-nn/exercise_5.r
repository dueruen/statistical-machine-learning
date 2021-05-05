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

classifier_rbf <- ksvm(V1~ ., data = id_train, kernel = "rbfdot", kpar=list(sigma=0.05), C = 1)




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

  network <- mlp(id_train[,-1], training_classes, size = size, maxit = 200, hiddenActFunc = "Act_Logistic", 
                 learnFunc="Std_Backpropagation", learnFuncParams = c(0.009,0))
  
  plotIterativeError(network)
  
  network$IterativeFitError[200]
  
  return(network)
}

network <- train_mlp(id_train, trainingClass, c(50,20))


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

acc_train <- evaluate_nn(network, id_train)
print("Accuracy when evaluated with training data:")
print(acc_train)
acc_test <- evaluate_nn(network, id_test)
print("Accuracy when evaluated with testing data:")
print(acc_test)


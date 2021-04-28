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
classifier_rbf <- ksvm(V1~ ., data = id_train, kernel = "polydot", kpar="automatic", C = 1)
end_time <- Sys.time()
run_time <- end_time - start_time

classifier_pre <- predict(classifier_rbf,id_test)

cf <- confusionMatrix(classifier_pre, id_test[,1])
print( sum(diag(cf$table))/sum(cf$table) )
print( run_time )

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

network <- mlp(id_train[,-1], trainingClass, size = c(20,20,20), maxit = 100, hiddenActFunc = "Act_TanH", 
               learnFunc="Std_Backpropagation", learnFuncParams = c(0.01,0))

plotIterativeError(network)
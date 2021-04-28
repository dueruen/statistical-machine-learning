library(rpart)
library(rpart.plot) #SKAL INSTALLERES
library(stats)
library(randomForest)
library(caret)

#Load the dataset
load("idList-cornered-100-2021.Rdata")

idList <- do.call(rbind,idList)
idList <- as.data.frame(idList)
idList[,1] <- factor(idList[,1])

#########
## 
#########






####################################
####### 5.2: Neural Networks ####### 
####################################

#######################################################################################################
## 5.2.1
## Create a matrix (as below in grey background) for the training classes. It has N rows (the same as 
## the number of training data) and 10 columns (binary). The column with '1' marks the 
## corresponding class as the example shown below (eg. cyper '0' is represented by '1000000000').
#######################################################################################################

lev <- levels(idList$V1) # Number of classes

# Create a list probabilities, for all labels
nnTrainingClass <- matrix(nrow = length(idList$V1), ncol = 10, data = 0) 

for(i in 1:length(idList$V1)) { # Set probabilities to one for matching class
  matchList <- match(lev,toString(idList$V1[i]))
  matchList[is.na(matchList)] <- 0
  nnTrainingClass[i,] <- matchList
}
trainingClass <- as.data.frame(nnTrainingClass)



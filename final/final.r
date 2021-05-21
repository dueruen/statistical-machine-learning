library(rpart)
library(rpart.plot) #SKAL INSTALLERES
library(stats)
library(randomForest)
library(caret)
library(kernlab)
library(RSNNS)

#Load the dataset
load("idList-FinalExam.Rdata")

id <- do.call(rbind, idList[])
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

items_per_person = nrow(id) / length(idList)
disjunct_train <- id[1:(items_per_person*30),]
disjunct_test <- id[(items_per_person*30 + 1):(items_per_person*38),]

dataset_shuffle <-id[sample(nrow(id)),]
all_persons_train <- dataset_shuffle[1:(items_per_person*30),]
all_persons_test <- dataset_shuffle[(items_per_person*30 + 1):(items_per_person*38),]

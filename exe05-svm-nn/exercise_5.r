library(rpart)
library(rpart.plot) #SKAL INSTALLERES
library(stats)
library(randomForest)
library(caret)
library(kernlab)

#Load the dataset
load("idList-cornered-100-2021.Rdata")

id <- do.call(rbind, idList[])
id <- as.data.frame(id)
id[,1] <- factor(id[,1])

items_per_person = nrow(id) / length(idList)
id_train <- id[1:(items_per_person*9),]
id_test <- id[(items_per_person*9 + 1):(items_per_person*13),]

#########
## 
#########

classifier_rbf <- ksvm(V1~ ., data = id_train, kernel = "rbfdot", kpar=list(sigma=0.05), C = 1)


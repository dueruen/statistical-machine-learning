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

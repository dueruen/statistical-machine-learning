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

set.seed(1234)
dataset_shuffle <-id[sample(nrow(id)),]
all_persons_train <- dataset_shuffle[1:(items_per_person*30),]
all_persons_test <- dataset_shuffle[(items_per_person*30 + 1):(items_per_person*38),]

id <- idList[[5]]
#for (num in 5:5) {
  img <- matrix(,180,360)
  
  for (x in 0:9){
    idx <- 0
    for (y in 1:20) {
      idx <- idx+1
      tmpM <- matrix(id[(x*200+idx),2:325],18,18)
      for (xM in 1:18) {
        for (yM in 1:18) {
          img[(x-1)*18+xM, (y-1)*18+yM] <- tmpM[xM,yM]
        }
        
      }
      
    }
    
  }
  
  image(img,col=gray(0:100/100) )
#}

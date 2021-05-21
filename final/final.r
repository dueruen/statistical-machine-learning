library(rpart)
library(rpart.plot) #SKAL INSTALLERES
library(stats)
library(randomForest)
library(caret)
library(kernlab)
library(RSNNS)

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
id_min_max_normalized <- cbind(V1 = 0, id_min_max_normalized)

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
id_z_normalized <- cbind(V1 = 0, id_z_normalized)

z_normalized_disjunct_train <- id_z_normalized[1:(items_per_person*30),]
z_normalized_disjunct_test <- id_z_normalized[(items_per_person*30 + 1):(items_per_person*38),]

set.seed(1234)
dataset_shuffle <-id_z_normalized[sample(nrow(id_z_normalized)),]
z_normalized_all_persons_train <- dataset_shuffle[1:(items_per_person*30),]
z_normalized_all_persons_test <- dataset_shuffle[(items_per_person*30 + 1):(items_per_person*38),]














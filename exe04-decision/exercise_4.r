library(rpart)
library(stats)

#Load the dataset
load("idList-cornered-100-2021.Rdata")

# Data to dataframe
idLoaded <- do.call(rbind, idList[1:13]) 
idLoaded <- as.data.frame(idLoaded)
idLoaded[,1] <- factor(idLoaded[,1])

# Remove factor column
dataset_no_labels <- idLoaded[,-1]

#########
## 4.1.1
## Compute the optimal decision point for the first 5 PCAs of a dataset (e.g. a single person) and 
## compute the information gain associated to it (plot 5 graphs, one for each component, and show the highest information gain)
#########
performPCA <- function(dataset){
  pca <- prcomp(dataset,scale=FALSE)
  return (pca)
}

pca <- performPCA(dataset_no_labels)

computeInformationGain <- function() { 
    
}



###########
## 4.1.2 
## Compute a decision tree for the digit classification and visualize it.
###########

# Code from Zhuoqi, which I think might actually be the entire solution?
datanew <- cbind(id_pca_first_5, id[,1]) # 'id_pca_first_5' is a dataframe
datanew$States <-factor(datanew[,6])
tree <- rpart(States ~ PC1 + PC2 + PC3 + PC4 + PC5, data = datanew, method = "class")
rpart.plot(tree)
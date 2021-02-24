library(stats)
#Don't print numbers as exponential notation
options("scipen"=100, "digits"=4)

#Load the dataset
load("idList-co-100.rdata")

#
idLoaded <- do.call(rbind, idList[1:10])
idLoaded <- as.data.frame(idLoaded)
idLoaded[,1] <- factor(idLoaded[,1])

dataset_no_labels <- idLoaded[,-1]
#Perform PCA on the dataset, and scale=TRUE because the variables should have standard deviation one. This is advisable.
pca <- prcomp(dataset_no_labels,scale=TRUE)

#Show the summary with standard deviation, proportion of variance and Cumulative Proportion
summary(pca)

#The variance explained by each principal component is obtained by squaring these:
pr.var=pca$sdev^2
pr.var
#To compute the proportion of variance explained by each principal component, 
# we simply divide the variance explained by each principal component by the total variance explained by all four principal components:
pve=pr.var/sum(pr.var)
pve

#A plot of the PVE explained by each component
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type="b")
#the cumulative PVE plotted. This shows the CDF and tells us when x% of variance is explained
plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type="b")

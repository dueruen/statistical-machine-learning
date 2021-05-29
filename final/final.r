library(rpart)
library(rpart.plot) #SKAL INSTALLERES
library(stats)
library(randomForest)
library(caret)
library(kernlab)
library(gmodels)
library(class)
library(caret)
library("spatstat")
library(rfUtilities)

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
      tmpM <- matrix(d[(x*200),2:325],18,18)
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
plotData2 <- function (data_to_plot) {
  id_mat <-data.matrix(data_to_plot, rownames.force = NA)
  img <- matrix(,180,180)
  cipher <- 1
  for (x in 1:10){
    for (y in 1:10) {
      rotated <-c(id_mat[-200+((x)*200)+1+(y*2000),2:ncol(id_mat)])
      rotated <-((rotated -min(rotated)) / (max(rotated) -min(rotated)))
      tmpM <-matrix(rotated,18,18)
      
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

#######
## Min max normalization
#######
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

id_min_max_normalized <- as.data.frame(lapply(id[-1], normalize))
id_min_max_normalized <- cbind(V1 = id[1], id_min_max_normalized)

plotData2 (id_min_max_normalized)

#######
## Z normalization
#######
id_z_normalized <- as.data.frame(scale(id[-1]))
id_z_normalized <- cbind(V1 = id[1], id_z_normalized)

plotData2 (id_z_normalized)

#######
## Gaussian smoothing
#######
smoothImage <- function(grayImg, sigma_value){
  smoothed <- as.matrix(blur(as.im(grayImg), sigma = sigma_value, normalise=FALSE, bleed = TRUE, varcov=NULL))
  return(smoothed)
}

gaussianSmoothing <- function (data_without_labels, sigma_value) {
  id_mat <- data.matrix(data_without_labels, rownames.force = NA)
  rotate <- function(x) t(apply(x, 2, rev))
  
  # Smooth all images
  for(i in 1:nrow(id_mat)) {
    rotated <- c(id_mat[i,2:ncol(data_without_labels)])
    image <- matrix(rotated,nrow = 18,ncol = 18, byrow = FALSE)
    image <- smoothImage(image, sigma_value)
    id_mat[i,2:ncol(id_mat)] <- matrix(image,nrow = 1,ncol = ncol(id_mat) - 1, byrow = FALSE)
  }
  idSmoothed <- as.data.frame(id_mat)
  #idSmoothed[,1] <- factor(id[,1]) #idSmoothed holds the smoothed image data
  return(idSmoothed)
}
gu_si_025 <- gaussianSmoothing(id[,-1], 0.25)
plotData2 (gu_si_025)

gu_si_05 <- gaussianSmoothing(id[,-1], 0.5)
plotData2 (gu_si_05)

gu_si_075 <- gaussianSmoothing(id[,-1], 0.75)
plotData2 (gu_si_075)

#######
## Principal Component Analysis (PCA)
#######
performPCA <- function(dataset){
  pca <- prcomp(dataset,scale=FALSE)
  return (pca)
}

get_prop_and_cum <- function(dataset) {
  raw_id_pca <- performPCA(dataset)
  
  eigs <- raw_id_pca$sdev^2
  Proportion = eigs/sum(eigs)
  Cumulative = cumsum(eigs)/sum(eigs)
  return(list(Proportion, Cumulative))
}
raw_pca_prop_and_cum <- get_prop_and_cum(id[,-1])

plot(raw_pca_prop_and_cum[[1]], type="o", xlab="Principal Component", ylab="Proportion of variance")
plot(raw_pca_prop_and_cum[[2]], type="o", xlab="Principal Component", ylab="Cumulative sum of variance")

##
# Plot eigens
##
plotEigen <- function (pca_data) {
  id_mat <-data.matrix(pca_data$rotation[], rownames.force = NA)
  img <- matrix(,180,180)
  cipher <- 1
  for (x in 1:10){
    for (y in 1:10) {
      rotated <-c(id_mat[,(((x - 1) * 10) + y)])
      rotated <-((rotated -min(rotated)) / (max(rotated) -min(rotated)))
      
      tmpM <-matrix(rotated,18,18)
      
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
raw_id_pca <- performPCA(id[,-1])
plotEigen(raw_id_pca)


##
# Combining
##

id_min_max_normalized <- as.data.frame(lapply(id[-1], normalize))
id_z_normalized <- as.data.frame(scale(id[-1]))
id_gau <- gaussianSmoothing(id[-1], 0.75)

#MIN MAX
min_max_gau <- gaussianSmoothing(id_min_max_normalized, 0.75)
plotData2 (min_max_gau)

min_max_gau_z <- as.data.frame(scale(min_max_gau))
plotData2 (min_max_gau_z)

min_max_z <- as.data.frame(scale(id_min_max_normalized))
plotData2 (min_max_z)

min_max_z_gau <- gaussianSmoothing(min_max_z, 0.75)
plotData2 (min_max_z_gau)

#With label
min_max_z_gau.label <- cbind(V1 = id[1], min_max_z_gau)


# GAU
gau_min_max <- as.data.frame(lapply(id_gau, normalize))
plotData2 (gau_min_max)

gau_min_max_z <- as.data.frame(scale(gau_min_max))
plotData2 (gau_min_max_z)

gau_z <- as.data.frame(scale(id_gau))
plotData2 (gau_z)

gau_z_min_max <- as.data.frame(lapply(gau_z, normalize))
plotData2 (gau_z_min_max)

#Z norm
z_min_max <- as.data.frame(lapply(id_z_normalized, normalize))
plotData2 (z_min_max)

z_min_max_gau <- gaussianSmoothing(z_min_max, 0.75)
plotData2 (z_min_max_gau)

z_gau <- gaussianSmoothing(id_z_normalized, 0.75)
plotData2 (z_gau)
z_gau.labels <- cbind(V1 = id[1], z_gau)

z_gau_min_max <- as.data.frame(lapply(z_gau, normalize))
plotData2 (z_gau_min_max)

#PCA
get_prop_and_cum_from_pca <- function(data_pca) {
  eigs <- data_pca$sdev^2
  Proportion = eigs/sum(eigs)
  Cumulative = cumsum(eigs)/sum(eigs)
  return(list(Proportion, Cumulative))
}
#MIN MAX
min_max_pca <- performPCA(id_min_max_normalized)
min_max_pca_prop_cum <- get_prop_and_cum_from_pca(min_max_pca)
min_max_gau_pca <- performPCA(min_max_gau)
min_max_gau_pca_prop_cum <- get_prop_and_cum_from_pca(min_max_gau_pca)
min_max_gau_z_pca <- performPCA(min_max_gau_z)
min_max_gau_z_pca_prop_cum <- get_prop_and_cum_from_pca(min_max_gau_z_pca)
min_max_z_pca <- performPCA(min_max_z)
min_max_z_pca_prop_cum <- get_prop_and_cum_from_pca(min_max_z_pca)
min_max_z_gau_pca <- performPCA(min_max_z_gau)
min_max_z_gau_pca_prop_cum <- get_prop_and_cum_from_pca(min_max_z_gau_pca)

plot(min_max_pca_prop_cum[[1]], type="b", col=1, lwd=1, pch=1, xlab="Principal Component", ylab="Proportion of variance", xlim=range(0,10), ylim=range(0,0.5))
plot_labels <- c("min_max_pca")
lines(min_max_gau_pca_prop_cum[[1]], type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- c("min_max_gau_pca")
lines(min_max_gau_z_pca_prop_cum[[1]], type="b", col=3, lwd=1, pch=3)
plot_labels[3] <- c("min_max_gau_z_pca")
lines(min_max_z_pca_prop_cum[[1]], type="b", col=4, lwd=1, pch=4)
plot_labels[4] <- c("min_max_z_pca")
lines(min_max_z_gau_pca_prop_cum[[1]], type="b", col=5, lwd=1, pch=5)
plot_labels[5] <- c("min_max_z_gau_pca")
lines(raw_pca_prop_and_cum[[1]], type="b", col=6, lwd=1, pch=6)
plot_labels[6] <- c("raw_data_pca")
legend("right",plot_labels, lwd=c(1), col=c(1:6), pch=c(1:6), y.intersp=1)

plot(min_max_pca_prop_cum[[2]], type="b", col=1, lwd=1, pch=1, xlab="Principal Component", ylab="Cumulative sum of variance", xlim=range(0,75))
plot_labels <- c("min_max_pca")
lines(min_max_gau_pca_prop_cum[[2]], type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- c("min_max_gau_pca")
lines(min_max_gau_z_pca_prop_cum[[2]], type="b", col=3, lwd=1, pch=3)
plot_labels[3] <- c("min_max_gau_z_pca")
lines(min_max_z_pca_prop_cum[[2]], type="b", col=4, lwd=1, pch=4)
plot_labels[4] <- c("min_max_z_pca")
lines(min_max_z_gau_pca_prop_cum[[2]], type="b", col=5, lwd=1, pch=5)
plot_labels[5] <- c("min_max_z_gau_pca")
lines(raw_pca_prop_and_cum[[2]], type="b", col=6, lwd=1, pch=6)
plot_labels[6] <- c("raw_data_pca")
legend("right",plot_labels, lwd=c(1), col=c(1:6), pch=c(1:6), y.intersp=1)

#Gau
gau_pca <- performPCA(id_gau)
gau_pca_prop_cum <- get_prop_and_cum_from_pca(gau_pca)
gau_min_max_pca <- performPCA(gau_min_max)
gau_min_max_pca_prop_cum <- get_prop_and_cum_from_pca(gau_min_max_pca)
gau_min_max_z_pca <- performPCA(gau_min_max_z)
gau_min_max_z_pca_prop_cum <- get_prop_and_cum_from_pca(gau_min_max_z_pca)
gau_z_pca <- performPCA(gau_z)
gau_z_pca_prop_cum <- get_prop_and_cum_from_pca(gau_z_pca)
gau_z_min_max_pca <- performPCA(gau_z_min_max)
gau_z_min_max_pca_prop_cum <- get_prop_and_cum_from_pca(gau_z_min_max_pca)

plot(gau_pca_prop_cum[[1]], type="b", col=1, lwd=1, pch=1, xlab="Principal Component", ylab="Proportion of variance", xlim=range(0,10), ylim=range(0,0.5))
plot_labels <- c("gau_pca")
lines(gau_min_max_pca_prop_cum[[1]], type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- c("gau_min_max_pca")
lines(gau_min_max_z_pca_prop_cum[[1]], type="b", col=3, lwd=1, pch=3)
plot_labels[3] <- c("gau_min_max_z_pca")
lines(gau_z_pca_prop_cum[[1]], type="b", col=4, lwd=1, pch=4)
plot_labels[4] <- c("gau_z_pca")
lines(gau_z_min_max_pca_prop_cum[[1]], type="b", col=5, lwd=1, pch=5)
plot_labels[5] <- c("gau_z_min_max_pca")
lines(raw_pca_prop_and_cum[[1]], type="b", col=6, lwd=1, pch=6)
plot_labels[6] <- c("raw_data_pca")
legend("right",plot_labels, lwd=c(1), col=c(1:6), pch=c(1:6), y.intersp=1)

plot(gau_pca_prop_cum[[2]], type="b", col=1, lwd=1, pch=1, xlab="Principal Component", ylab="Cumulative sum of variance", xlim=range(0,75))
plot_labels <- c("gau_pca")
lines(gau_min_max_pca_prop_cum[[2]], type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- c("gau_min_max_pca")
lines(gau_min_max_z_pca_prop_cum[[2]], type="b", col=3, lwd=1, pch=3)
plot_labels[3] <- c("gau_min_max_z_pca")
lines(gau_z_pca_prop_cum[[2]], type="b", col=4, lwd=1, pch=4)
plot_labels[4] <- c("gau_z_pca")
lines(gau_z_min_max_pca_prop_cum[[2]], type="b", col=5, lwd=1, pch=5)
plot_labels[5] <- c("gau_z_min_max_pca")
lines(raw_pca_prop_and_cum[[2]], type="b", col=6, lwd=1, pch=6)
plot_labels[6] <- c("raw_data_pca")
legend("right",plot_labels, lwd=c(1), col=c(1:6), pch=c(1:6), y.intersp=1)

#Z
z_pca <- performPCA(id_z_normalized)
z_pca_prop_cum <- get_prop_and_cum_from_pca(z_pca)
z_min_max_pca <- performPCA(z_min_max)
z_min_max_pca_prop_cum <- get_prop_and_cum_from_pca(z_min_max_pca)
z_min_max_gau_pca <- performPCA(z_min_max_gau)
z_min_max_gau_pca_prop_cum <- get_prop_and_cum_from_pca(z_min_max_gau_pca)
z_gau_pca <- performPCA(z_gau)
z_gau_pca_prop_cum <- get_prop_and_cum_from_pca(z_gau_pca)
z_gau_min_max_pca <- performPCA(z_gau_min_max)
z_gau_min_max_pca_prop_cum <- get_prop_and_cum_from_pca(z_gau_min_max_pca)

plot(z_pca_prop_cum[[1]], type="b", col=1, lwd=1, pch=1, xlab="Principal Component", ylab="Proportion of variance", xlim=range(0,10), ylim=range(0,0.5))
plot_labels <- c("z_pca")
lines(z_min_max_pca_prop_cum[[1]], type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- c("z_min_max_pca")
lines(z_min_max_gau_pca_prop_cum[[1]], type="b", col=3, lwd=1, pch=3)
plot_labels[3] <- c("z_min_max_gau_pca")
lines(z_gau_pca_prop_cum[[1]], type="b", col=4, lwd=1, pch=4)
plot_labels[4] <- c("z_gau_pca")
lines(z_gau_min_max_pca_prop_cum[[1]], type="b", col=5, lwd=1, pch=5)
plot_labels[5] <- c("z_gau_min_max_pca")
lines(raw_pca_prop_and_cum[[1]], type="b", col=6, lwd=1, pch=6)
plot_labels[6] <- c("raw_data_pca")
legend("right",plot_labels, lwd=c(1), col=c(1:6), pch=c(1:6), y.intersp=1)

plot(z_pca_prop_cum[[2]], type="b", col=1, lwd=1, pch=1, xlab="Principal Component", ylab="Cumulative sum of variance", xlim=range(0,75))
plot_labels <- c("z_pca")
lines(z_min_max_pca_prop_cum[[2]], type="b", col=2, lwd=1, pch=2)
plot_labels[2] <- c("z_min_max_pca")
lines(z_min_max_gau_pca_prop_cum[[2]], type="b", col=3, lwd=1, pch=3)
plot_labels[3] <- c("z_min_max_gau_pca")
lines(z_gau_pca_prop_cum[[2]], type="b", col=4, lwd=1, pch=4)
plot_labels[4] <- c("z_gau_pca")
lines(z_gau_min_max_pca_prop_cum[[2]], type="b", col=5, lwd=1, pch=5)
plot_labels[5] <- c("z_gau_min_max_pca")
lines(raw_pca_prop_and_cum[[2]], type="b", col=6, lwd=1, pch=6)
plot_labels[6] <- c("raw_data_pca")
legend("right",plot_labels, lwd=c(1), col=c(1:6), pch=c(1:6), y.intersp=1)

plotEigen(min_max_gau_pca)

###########
## Random forests 
## Create a Random Forest classifier and evaluate it using cross validation.
###########
# Resources
# https://www.blopig.com/blog/2017/04/a-very-basic-introduction-to-random-forests-using-r/
# https://rpubs.com/Gonzo25/94920
# https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/rfcv

# Logging to get to know random forest
#model
# Using the importance() function, we can view the importance of each variable.
#importance(model)

#######
## Find the best amount of PC's to use
#######
performPCA <- function(dataset, pcaCount){
  pca <- prcomp(dataset,scale=TRUE, rank = pcaCount)
  return (pca)
}

pca <- prcomp(id,scale=FALSE)
summary(pca)

calculateExplainedVariance <- function(pca){
  #The variance explained by each principal component is obtained by squaring these:
  pr.var=pca$sdev^2 
  return (pr.var)
}
pr.var <- calculateExplainedVariance(pca)
# To compute the proportion of variance explained by each principal component, 
# we simply divide the variance explained by each principal component by the total variance explained by all four principal components:
calculatePropOfVariance <- function(pr.var){
  pve=pr.var/sum(pr.var)
  return (pve)
}

pve <- calculatePropOfVariance(pr.var)
pve

#####
## Calculate the right amount of PCs
#####

#Right amount of PCs
pca.all.gau <- prcomp(min_max_z_gau.label[,-1], scale = TRUE)
pca.all.raw <- prcomp(id[,-1], scale = TRUE)
pca.all.normalized <- prcomp(id_min_max_normalized, scale = TRUE)
pca.all.minmaxgau <- prcomp(min_max_gau, scale = TRUE)

pr.var.all.gau <- calculateExplainedVariance(pca.all.gau)
pr.var.all.raw <- calculateExplainedVariance(pca.all.raw)
pr.var.all.normalized <- calculateExplainedVariance(pca.all.normalized)
pr.var.all.minmaxgau <- calculateExplainedVariance(pca.all.minmaxgau)

plot(pr.var.all.raw[1:70], xlab="Principal Component", ylab="Variance", ylim=c(0,150),type="b")
abline(v = 41, col="green", lty=5)
abline(h = 1, col="red", lty=5)

pve.all.gau <- calculatePropOfVariance(pr.var.all.gau)
pve.all.raw <- calculatePropOfVariance(pr.var.all.raw)
pve.all.normalized <- calculatePropOfVariance(pr.var.all.normalized)
pve.all.minmaxgau <- calculatePropOfVariance(pr.var.all.minmaxgau)

total.variance <- 0.90

in_pc.gau <- cumsum(pve.all.gau) <= total.variance #sum values up to the explained variance
in_pc.raw <- cumsum(pve.all.raw) <= total.variance #sum values up to the explained variance
in_pc.normalized <- cumsum(pve.all.normalized) <= total.variance #sum values up to the explained variance
in_pc.minmaxgau <- cumsum(pve.all.minmaxgau) <= total.variance #sum values up to the explained variance

sum(in_pc.gau) #Count true values gau
sum(in_pc.raw) #Count true values raw
sum(in_pc.normalized) #Count true values normalized
sum(in_pc.minmaxgau) #Count true values minmaxgau


#A plot of the PVE explained by each component
plot(pve[1:10], xlab="Principal Component", ylab="Proportion of Variance Explained",type="b")
abline(v = 8, col="red", lty=5)

#the cumulative PVE plotted. This shows the CDF and tells us when x% of variance is explained. Only take the first 100 PCA's
plot(cumsum(pve[1:100]), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type="b")

#Transform data
transform.data <- function(dataset, pca){
  cat("HERE")
  transformed.dataset <- predict(pca, newdata = dataset)
  cat("transformed.dataset:", nrow(transformed.dataset), sep = " ")
  # Convert to transformed dataset to dataframe
  df.transformed.dataset <- as.data.frame(transformed.dataset) 
  return (df.transformed.dataset)
}
###
#Cross validation for evaluating random forest
##

cross_validation_random_forest <- function(data_set, seed, numberoftrees) {
  set.seed(seed)
  folds <-createFolds(data_set[, 1], k = 10) #data_set$X1
  
  tree.count <- numberoftrees
  
  #model <- randomForest(States ~ PC1 + PC2 + PC3 + PC4 + PC5, data = data_set, ntree=tree.count)
  
  cross_validation_results <- lapply(folds, function(x) {
    
    #90% of the entire dataset is assigned to data_train
    data_train_with_labels <- data_set[-x, ]
    data_train <- data_train_with_labels[, -1]
    data_train_labels <- data_train_with_labels[ , 1]
    
    # The rest 10% of the dataset is assigned to data_test
    data_test_with_labels <- data_set[x, ]
    data_test <- data_test_with_labels[, -1]
    data_test_labels <- data_test_with_labels[ , 1]
    
    #Perform PCA but with only with 5 principal components
    pca.5 <- performPCA(data_train, 5)
    
    #Take one person and transform dataset and add States column
    df.transformed.dataset <- transform.data(data_train_with_labels, pca.5)
    datanew <- cbind(df.transformed.dataset, data_train_with_labels[,1]) # 'df.transformed.dataset' is a dataframe
    datanew$States <- factor(datanew[,6])
    
    start_time <- Sys.time()
    # Create Random Forest classifer. Uses single person.
    model <- randomForest(States ~ ., data = datanew, ntree=tree.count)
    end_time <- Sys.time()
    
    df.transformed.test_dataset <- transform.data(data_test_with_labels, pca.5)
    datanew_test <- cbind(df.transformed.test_dataset, data_train_with_labels[,1]) # 'df.transformed.dataset' is a dataframe
    datanew_test$States <- factor(datanew_test[,6])
    
    #Predict random vs. test set
    data_pred <-predict(model,datanew_test)
    
    run_time <- end_time - start_time
    
    accuracy = mean(data_pred == data_test_labels)
    
    cat("Accuracy:", accuracy, sep = "\n")
    
    return (c(accuracy, run_time))
  })
  return(cross_validation_results)
}

###
##Optimized CR
###

folds <- lapply(c(0,3,7,11,13,15,19,24,28,30), function(x) {
  startVal <- 1 + (x * 2000)
  endVal <- (startVal + (4 * 2000)) - 1
  return(startVal:endVal)
})

optimized_cross_validation_random_forest <- function(data_set, seed, numberoftrees, principal_components) {
  set.seed(seed)
  folds <- lapply(c(0,3,7,11,13,15,19,24,28,30), function(x) {
    startVal <- 1 + (x * 2000)
    endVal <- (startVal + (4 * 2000)) - 1
    return(startVal:endVal)
  })
  #folds <-createFolds(data_set[, 1], k = 10) 
  tree.count <- numberoftrees
  
  cross_validation_results <- lapply(folds, function(x) {
    #cat("Fold:", x, sep = " ")
    #90% of the entire dataset is assigned to data_train
    data_train_with_labels <- data_set[-x, ]
    data_train <- data_train_with_labels[, -1]
    data_train_labels <- data_train_with_labels[ , 1]
    data_train_with_labels[ , 1]
    
    # The rest 10% of the dataset is assigned to data_test
    data_test_with_labels <- data_set[x, ]
    data_test <- data_test_with_labels[, -1]
    data_test_labels <- data_test_with_labels[ , 1]
    
    #Perform PCA
    pca <- performPCA(data_train, principal_components)
    #pca.this <- pca$x[,1:principal_components]
    #pca.this <- data.frame(pca.this) # Make it a data frame
    
    #Take one person and transform dataset and add States column
    df.transformed.dataset <- transform.data(data_train_with_labels, pca)
    datanew <- cbind(df.transformed.dataset, data_train_with_labels[,1]) # 'df.transformed.dataset' is a dataframe
    datanew$States <- factor(datanew[,principal_components + 1])
    #datanew$`data_train_with_labels[,1]` <- NULL
    
    start_time <- Sys.time()
    # Create Random Forest classifer. 
    model <- randomForest(States ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = datanew, ntree=tree.count)
    #model <- randomForest(States ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21, data = datanew, ntree=tree.count)
    run_time <- difftime(Sys.time(), start_time, units = "secs")
    
    df.transformed.test_dataset <- transform.data(data_test_with_labels, pca)
    datanew_test <- cbind(df.transformed.test_dataset, data_test_with_labels[,1]) # 'df.transformed.dataset' is a dataframe
    datanew_test$States <- factor(datanew_test[,principal_components + 1])
    #datanew_test$`data_train_with_labels[,1]` <- NULL
    
    #Predict random vs. test set
    data_pred_test <-predict(model,datanew_test)
    
    df.transformed.train_dataset <- transform.data(data_train_with_labels, pca)
    datanew_train <- cbind(df.transformed.train_dataset, data_train_with_labels[,1]) # 'df.transformed.dataset' is a dataframe
    datanew_train$States <- factor(datanew_train[,principal_components + 1])
    
    data_pred_train <-predict(model,datanew_train)
    
    accuracy.test = mean(data_pred_test == data_test_labels)
    accuracy.train = mean(data_pred_train == data_train_labels)
    
    #cat("Accuracy test:", accuracy.test, sep = "\n")
    #cat("Accuracy train:", accuracy.train, sep = "\n")
    
    return (c(accuracy.test, accuracy.train, run_time))
  })
  return(cross_validation_results)
}

ns <- function () {
  ns_result <- lapply(c(1,50,100,300), function(x) {
    cat("Tree:", x, sep = "\n")
    data <- optimized_cross_validation_random_forest(id,123, x, 10)
    data <- do.call(rbind, data[])
    data <- as.data.frame(data)
    variance.test <- var(data[,1])
    variance.train <- var(data[,2])
    
    mean.test <- mean(data[,1])
    mean.train <- mean(data[,2])
    
    str_dev.test <- sqrt(variance.test)
    #cat("str_dev.test:", str_dev.test, sep = "\n")
    #cat("(mean.test + str_dev.test):", (mean.test + str_dev.test), sep = "\n")
    str_dev.train <- sqrt(variance.train)
    
    mean.runtime <- mean(data[,3])
    
    return(c(mean.test, (mean.test + str_dev.test), (mean.test - str_dev.test), mean.train, (mean.train + str_dev.train), (mean.train - str_dev.train), mean.runtime))
  })
  return(ns_result)
}

res_crossVa <- ns()

plot_rf_data <- function(data_to_plot) {
  rf_plot_data <- do.call(rbind, data_to_plot[])
  rf_plot_data  <- as.data.frame(rf_plot_data )
  x_range <- c(1,50,100,300)
  plot(x_range, rf_plot_data[,1], type="b", col=3, lwd=3, pch=1, xlab="Trees", ylab="Mean Accuracy [%]", ylim=range(0.5,1))
  plot_labels <- c("Mean Accuracy test")
  #lines(x_range, rf_plot_data[,2], type="b", col=2, lwd=1)
  #lines(x_range, rf_plot_data[,3], type="b", col=2, lwd=1)
  polygon(c(x_range, rev(x_range)), c(rf_plot_data[,2], rev(rf_plot_data[,3])), col = adjustcolor("red",alpha.f=0.2) )
  lines(x_range, rf_plot_data[,4], type="b", col=4, lwd=3, pch=1)
  plot_labels[[2]] <- c("Mean Accuracy train")
  #lines(x_range, rf_plot_data[,5], type="b", col=2, lwd=1)
  #lines(x_range, rf_plot_data[,6], type="b", col=2, lwd=1)
  polygon(c(x_range, rev(x_range)), c(rf_plot_data[,5], rev(rf_plot_data[,6])), col = adjustcolor("red",alpha.f=0.2) )
  par(new = TRUE)
  plot(x_range, rf_plot_data[,7], type = "b", col=1,lwd=1, pch=1, axes = FALSE, bty = "n", xlab = "", ylab = "")
  axis(side=4, at = pretty(range(rf_plot_data[,7])))
  mtext("Mean run time / [s]", side=4, line=3)
  plot_labels[[3]] <- c("Mean runtime")
  legend("bottomright",plot_labels, lwd=c(3,3,1), col=c(3,4,1), pch=c(1,1,1), y.intersp=1)
  title("All persons raw data")
}

plot_rf_data(res_crossVa)

##
#The zouqi way
##

#Preparing PCA
pca.5 <- performPCA(id[,-1], 31)

#Take one person and transform dataset and add States column
df.transformed.dataset <- transform.data(data_train_with_labels, pca.5)
datanew <- cbind(df.transformed.dataset, data_train_with_labels[,1]) # 'df.transformed.dataset' is a dataframe
datanew$States <- factor(datanew[,6])

start_time <- Sys.time()
# Create Random Forest classifer. Uses single person.
model <- randomForest(States ~ ., data = datanew, ntree=tree.count)
end_time <- Sys.time()



pca.10 <- pca.5$x[,1:31]
pca.10 <- data.frame(pca.10) # Make it a data frame

datanew <- cbind(pca.10, id[,1])
datanew$States <-factor(datanew[,32])
datanew$`id[, 1]` <- NULL
model.random <- randomForest(States ~ ., data = datanew, ntree=500)

p.disjunkt <- predict(model.random.100.pc.train,raw_disjunct_test)
cf.disjunkt <- confusionMatrix(p.disjunkt, raw_disjunct_test[,1])
print( sum(diag(cf$table))/sum(cf$table) )

#Test 100 trees. PCA Changing nodesize didn't have impact but maxnodes has big impact on computation and error rate.
start_time <- Sys.time()
model.random.100 <- randomForest(States ~ . , data = datanew, ntree=100, maxnodes = 59)
end_time <- Sys.time()
run_time.100 <- end_time - start_time

#Test 500 trees PCA
start_time <- Sys.time()
model.random.500 <- randomForest(States ~ . , data = datanew, ntree=500)
end_time <- Sys.time()
run_time.500 <- end_time - start_time

#Random forest disjunct 100 trees
start_time <- Sys.time()
model.random.100.disjunct.train <- randomForest(V1 ~ . , data = raw_disjunct_train, ntree=100)
end_time <- Sys.time()
run_time.100.disjunct <- end_time - start_time

p.disjunkt <- predict(model.random.100.disjunct.train,raw_disjunct_test)
cf.disjunkt <- confusionMatrix(p.disjunkt, raw_disjunct_test[,1])
print( sum(diag(cf$table))/sum(cf$table) )

#start_time <- Sys.time()
#model.random.100.disjunct <- randomForest(V1 ~ . , data = id, ntree=100)
#end_time <- Sys.time()
#run_time.100.disjunct <- end_time - start_time

#Random forest all-persons in 100 trees
start_time <- Sys.time()
model.random.100.raw_all_person.train <- randomForest(V1 ~ . , data = raw_all_persons_train, ntree=100)
end_time <- Sys.time()
run_time.100.raw_all_person <- end_time - start_time

p.all_person <- predict(model.random.100.raw_all_person.train,raw_all_persons_test)
cf.all_person <- confusionMatrix(p.all_person, raw_all_persons_test[,1])
print( sum(diag(cf$table))/sum(cf$table) )

#start_time <- Sys.time()
#model.random.100.all_person_in <- randomForest(V1 ~ . , data = dataset_shuffle, ntree=100)
#end_time <- Sys.time()
#run_time.100.all_person_in <- end_time - start_time

#Prep
model.random.500
#OOB + confusion matrix + disjunkt
model.random.100.disjunct
#OOB + confusion matrix + disjunkt
model.random.100.all_person_in

par(mfrow=c(1,1)) 
plot(model.random.100, main ="Error as a function of trees") # Error as a function of trees
legend("topright", colnames(model.random$err.rate),col=1:1, cex=0.8,fill=1:12)

plot(model.random.500, main ="Error as a function of trees") # Error as a function of trees
legend("topright", colnames(model.random$err.rate),col=1:1, cex=0.8,fill=1:12)

plot(model.random.100.disjunct, main ="Error as a function of trees") # Error as a function of trees
legend("topright", colnames(model.random$err.rate),col=1:1, cex=0.8,fill=1:12)

# 10-fold cross validation

model.random.100.raw_all_person.train <- randomForest(V1 ~ . , data = raw_all_persons_train, ntree=100)

start_time <- Sys.time()
model.cv <- rf.crossValidation(x = model.random.100.raw_all_person.train, xdata = raw_all_persons_train, p = 0.1, n = 10, trace = TRUE) 
end_time <- Sys.time()
run_time <- end_time - start_time #Time difference of 4.699376 hours 1 run

# Plot cross validation verses model producers accuracy
par(mfrow=c(1,2)) 
plot(model.cv, type = "cv", main = "CV producers accuracy")
plot(model.cv, type = "model", main = "Model producers accuracy")

# Plot cross validation verses model OOB
par(mfrow=c(1,2)) 
plot(model.cv, type = "cv", stat = "oob", main = "CV OOB error", xlim=range(0,20), ylim=range(0,0.022))
plot(model.cv, type = "model", stat = "oob", main = "Model OOB error", xlim=range(0,20), ylim=range(0,0.022))


#Runs with dataset not preprocessed
res_cross_validation_with_random_forest.100 <- cross_validation_random_forest(dataset_shuffle,123, 100)
res_cross_validation_with_random_forest.500 <- cross_validation_random_forest(dataset_shuffle,123, 500)

res_random_forest.100.not.processed <- do.call(rbind, res_cross_validation_with_random_forest.100[])
res_random_forest.100.not.processed <- as.data.frame(res_random_forest.100.not.processed)

res_random_forest.500.not.processed <- do.call(rbind, res_cross_validation_with_random_forest.500[])
res_random_forest.500.not.processed <- as.data.frame(res_random_forest.500.not.processed)

#Cross-validation with preprocessed dataset (min_max_norm)
res_cross_validation_with_random_forest.100.processed.min_max_norm <- cross_validation_random_forest(id_min_max_normalized,123, 100)
res_cross_validation_with_random_forest.500.processed.min_max_norm <- cross_validation_random_forest(id_min_max_normalized,123, 500)

res_random_forest.100.min_max_norm <- do.call(rbind, res_cross_validation_with_random_forest.100.processed.min_max_norm[])
res_random_forest.100.min_max_norm <- as.data.frame(res_random_forest.100.min_max_norm)

res_random_forest.500.min_max_norm <- do.call(rbind, res_cross_validation_with_random_forest.500.processed.min_max_norm[])
res_random_forest.500.min_max_norm <- as.data.frame(res_random_forest.500.min_max_norm)


#Cross-validation with preprocessed dataset (min_max_z_gau)
res_cross_validation_with_random_forest.100.processed.min_max <- cross_validation_random_forest(min_max_z_gau.label,123, 100)
res_cross_validation_with_random_forest.500.processed.min_max <- cross_validation_random_forest(min_max_z_gau.label,123, 500)

res_random_forest.100.min_max_z_gau <- do.call(rbind, res_cross_validation_with_random_forest.100.processed.min_max[])
res_random_forest.100.min_max_z_gau <- as.data.frame(res_random_forest.100.min_max_z_gau)

res_random_forest.500.min_max_z_gau <- do.call(rbind, res_cross_validation_with_random_forest.500.processed.min_max[])
res_random_forest.500.min_max_z_gau <- as.data.frame(res_random_forest.500.min_max_z_gau)

#Plot only 100
boxplot(res_random_forest.100.min_max_z_gau[[1]], names=c("100 trees"), main="Accuracy")
boxplot(res_random_forest.100[[2]], names=c("100 trees"), main="Run time in s")

#Big plot
boxplot(
  res_random_forest.100.not.processed[[1]],
  res_random_forest.100.min_max_norm[[1]],
  res_random_forest.100.min_max_z_gau[[1]],
  res_random_forest.500.not.processed[[1]],
  res_random_forest.500.min_max_norm[[1]],
  res_random_forest.500.min_max_z_gau[[1]], 
  names=c("100t. not process","100t. minmaxnorm", "100t. gau","500t. not process","500t. minmaxnorm", "500t. gau"), 
  main="Accuracy"
)
boxplot(res_random_forest.100.not.processed[[2]],res_random_forest.100.min_max_norm[[2]],res_random_forest.100.min_max_z_gau[[2]], names=c("100t. not process","100t. minmaxnorm", "100t. gau"), main="Run time in sec")

#Plot both 100 and 500 
boxplot(res_random_forest.100[[1]],res_random_forest.500[[1]], names=c("100 trees","500 trees"), main="Accuracy")
boxplot(res_random_forest.100[[2]],res_random_forest.500[[2]], names=c("100 trees","500 trees"), main="Run time in s")



x <- 1:10
y <- rnorm(10)
## second data set on a very different scale
z <- runif(10, min=1000, max=10000) 
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(x, y) # first plot
par(new = TRUE)
plot(x, z, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(z)))
mtext("z", side=4, line=3)


###########################################################################
########################### K N N section #################################
###########################################################################




###########
## Cross-validate KNN, 10-folds
###########
cross_validation_knn <- function(data_set, seed, k_value, disjunct = FALSE) {
  set.seed(seed)
  
  if(disjunct == FALSE) {
    folds <-createFolds(data_set[, 1], k = 10) 
  }
  
  if(disjunct == TRUE) {
    folds <- lapply(c(0,3,7,11,13,15,19,24,28,30), function(x) {
      startVal <- 1 + (x * 2000)
      endVal <- (startVal + (4 * 2000)) - 1
      return(startVal:endVal)
    })
  }
  
  accuracyList <- c(1:10)
  runtimeList <- c(1:10)

  for(i in 1:10)
  {
    id_train <- data_set[-folds[[i]],-1]
    id_test <- data_set[folds[[i]],-1]
    
    id_train_labels <- data_set[-folds[[i]],1]
    id_test_labels <- data_set[folds[[i]],1]
    
    start_time <- Sys.time()
    id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=k_value)
    run_time <- difftime(Sys.time(), start_time, units = "secs")
    
    cf <- confusionMatrix(id_test_labels, id_test_pred)
    accuracyList[i] <- sum(diag(cf$table))/sum(cf$table)
    runtimeList[i] <- run_time

    
  }
  
  meanRuntime <- mean(runtimeList)
  varianceRuntime <- var(runtimeList)
  meanAccuracy <- mean(accuracyList)
  varianceAccuracy <- var(accuracyList)
  
  return(c(meanAccuracy, varianceAccuracy, meanRuntime, varianceRuntime))
    
}
###########
## Cross-validate KNN with PCA, 10-folds
###########
cross_validation_knn_pca <- function(data_set, seed, k_value, pca_count, disjunct = FALSE) {
  set.seed(seed)
  
  if(disjunct == FALSE) {
    folds <-createFolds(data_set[, 1], k = 10) 
  }
  
  if(disjunct == TRUE) {
    folds <- lapply(c(0,3,7,11,13,15,19,24,28,30), function(x) {
      startVal <- 1 + (x * 2000)
      endVal <- (startVal + (4 * 2000)) - 1
      return(startVal:endVal)
    })
  }
  
  accuracyList <- c(1:10)
  accuracyList.training <- c(1:10)
  runtimeList <- c(1:10)
  
  for(i in 1:10) {
    id_train <- data_set[-folds[[i]],-1]
    id_test <- data_set[folds[[i]],-1]
    
    id_train_labels <- data_set[-folds[[i]],1]
    id_test_labels <- data_set[folds[[i]],1]
    
    # Zhuoqi's code
    id_pca <- prcomp(id_train, center = TRUE, scale. = TRUE, rank. = pca_count)
    
    test_pca <- predict(id_pca,id_test)
    
    id_train <- id_pca$x
    id_test <- test_pca
    # End 
    
    start_time <- Sys.time()
    id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=k_value)
    run_time <- difftime(Sys.time(), start_time, units = "secs")
    
    id_test_pred.training <- knn(train = id_train, test = id_train, cl = id_train_labels, k=k_value)
    
    cf.training <- confusionMatrix(id_train_labels, id_test_pred.training)
    
    cf <- confusionMatrix(id_test_labels, id_test_pred)
    accuracyList[i] <- sum(diag(cf$table))/sum(cf$table)
    accuracyList.training[i] <- sum(diag(cf.training$table))/sum(cf.training$table)
    runtimeList[i] <- run_time
    
    
  }
  
  meanRuntime <- mean(runtimeList)
  varianceRuntime <- var(runtimeList)
  meanAccuracy <- mean(accuracyList)
  varianceAccuracy <- var(accuracyList)
  meanAccuracy.training <- mean(accuracyList.training)
  varianceAccuracy.training <- var(accuracyList.training)
  
  return(c(meanAccuracy, varianceAccuracy, meanRuntime,  meanAccuracy.training, varianceAccuracy.training))
}


###########
## Creates 2 lists containing standard deviation from mean, based on variance
###########
std_dev_lists <- function(variance_list_input, mean_list_input) { 
  
  upper <- c()
  lower <- c()
  
  variance_list <- unlist(variance_list_input)
  mean_list <- unlist(mean_list_input)
  length <- length(mean_list)
  
  for(i in 1:length) { 
    variance <- variance_list[[i]][1]
    mean <- mean_list[[i]][1]
    std_dev <- sqrt(variance)
    upper[i] <- mean + std_dev
    lower[i] <- mean - std_dev
    
  }
  
  result <- list(upper, lower)
  return(result)
}


############################################
# Plots results of CV on KNN for 10 k values
############################################
knn_cv_and_plot <- function(dataset, description, disjunct = FALSE) { 
  
  accuracyList <- c()
  runtimeList <- c()
  varianceList <- c()
  resultList <- c()
  for (i in seq(1,101, by = 10)) { 
    resultList[[i]] <- cross_validation_knn(dataset, 1234, i, disjunct)
    accuracyList[[i]] <- resultList[[i]][1]
    varianceList[[i]] <- resultList[[i]][2]
    runtimeList[[i]] <- resultList[[i]][3]
  } 
  
  std_dev <- std_dev_lists(varianceList, accuracyList)
  upper <- std_dev[[1]]
  lower <- std_dev[[2]]
  
  plot(c(1,11,21,31,41,51,61,71,81,91,101), unlist(accuracyList), type="b", col=3, lwd=3, pch=1, xlab="K value", ylab="Mean Accuracy [%]")
  plot_labels <- c("Mean Accuracy")
  lines(c(1,11,21,31,41,51,61,71,81,91,101), unlist(upper), type="b", col=2, lwd=1, pch=2)
  plot_labels[2] <- paste("Std. Deviation")
  lines(c(1,11,21,31,41,51,61,71,81,91,101), unlist(lower), type="b", col=2, lwd=1, pch=2)
  plot_labels[3] <- paste("Std. Deviation")
  polygon(c(c(1,11,21,31,41,51,61,71,81,91,101), rev(c(1,11,21,31,41,51,61,71,81,91,101))), c(upper, rev(lower)), col = adjustcolor("red",alpha.f=0.2) )
  legend("bottomleft",plot_labels, lwd=c(1), col=c(3,2,2), pch=c(1,2,2), y.intersp=1)
  header <- paste("Accuracy, ",description, sep="")
  title(header)
  
  plot(c(1,11,21,31,41,51,61,71,81,91,101), unlist(runtimeList), type="b", col=1, lwd=1, pch=1, xlab="K value", ylab="Mean Runtime [s]")
  header <- paste("Runtime, ",description, sep="")
  title(header)
  
}

##################################################
# Plots results of CV on KNN for 3 k values
##################################################
knn_cv_and_plot_small_run <- function(dataset, description, pca_count, disjunct = FALSE) { 
  
  accuracyList <- c()
  runtimeList <- c()
  varianceList <- c()
  resultList <- c()
  for (i in seq(1,101, by = 50)) { 
    resultList[[i]] <- cross_validation_knn(dataset, 1234, i, disjunct)
    accuracyList[[i]] <- resultList[[i]][1]
    varianceList[[i]] <- resultList[[i]][2]
    runtimeList[[i]] <- resultList[[i]][3]
  } 
  
  std_dev <- std_dev_lists(varianceList, accuracyList)
  upper <- std_dev[[1]]
  lower <- std_dev[[2]]
  
  plot(c(1,51,101), unlist(accuracyList), type="b", col=3, lwd=3, pch=1, xlab="K value", ylab="Mean Accuracy [%]")
  plot_labels <- c("Mean Accuracy")
  lines(c(1,51,101), unlist(upper), type="b", col=2, lwd=1, pch=2)
  plot_labels[2] <- paste("Std. Deviation")
  lines(c(1,51,101), unlist(lower), type="b", col=2, lwd=1, pch=2)
  plot_labels[3] <- paste("Std. Deviation")
  polygon(c(c(1,51,101), rev(c(1,51,101))), c(upper, rev(lower)), col = adjustcolor("red",alpha.f=0.2) )
  legend("bottomleft",plot_labels, lwd=c(1), col=c(3,2,2), pch=c(1,2,2), y.intersp=1)
  header <- paste("Accuracy, ",description, sep="")
  title(header)
  
  plot(c(1,51,101), unlist(runtimeList), type="b", col=1, lwd=1, pch=1, xlab="K value", ylab="Mean Runtime [s]")
  header <- paste("Runtime, ",description, sep="")
  title(header)
}


##################################################
# Plots results of CV on KNN w/ PCA for 3 k values
##################################################
knn_cv_and_plot_pca <- function(dataset, description, pca_count, disjunct = FALSE) { 
  
  accuracyList <- c()
  runtimeList <- c()
  varianceList <- c()
  resultList <- c()
  accuracyList.training <- c()
  varianceList.training <- c()
  for (i in seq(1,101, by = 50)) { 
    resultList[[i]] <- cross_validation_knn_pca(dataset, 1234, i, pca_count, disjunct)
    accuracyList[[i]] <- resultList[[i]][1]
    varianceList[[i]] <- resultList[[i]][2]
    runtimeList[[i]] <- resultList[[i]][3]
    accuracyList.training[[i]] <- resultList[[i]][4]
    varianceList.training[[i]] <- resultList[[i]][5]
  } 
  
  std_dev <- std_dev_lists(varianceList, accuracyList)
  upper <- std_dev[[1]]
  lower <- std_dev[[2]]
  
  std_dev.training <- std_dev_lists(varianceList.training, accuracyList.training)
  upper.training <- std_dev.training[[1]]
  lower.training <- std_dev.training[[2]]
  
  ## Test data
  plot(c(1,51,101), unlist(accuracyList.training), type="b", col=4, lwd=3, pch=3, xlab="K value", ylab="Mean Accuracy [%]")
  plot_labels <- c("Mean Accuracy Training")
  lines(c(1,51,101), unlist(upper.training), type="b", col=2, lwd=1, pch=4)
  plot_labels[2] <- paste("Std. Deviation Training")
  lines(c(1,51,101), unlist(lower.training), type="b", col=2, lwd=1, pch=4)
  plot_labels[3] <- paste("Std. Deviation Training")
  polygon(c(c(1,51,101), rev(c(1,51,101))), c(upper.training, rev(lower.training)), col = adjustcolor("red",alpha.f=0.2) )
  lines(c(1,51,101), unlist(accuracyList), type="b", col=3, lwd=3, pch=1)
  plot_labels[4] <- c("Mean Accuracy")
  lines(c(1,51,101), unlist(upper), type="b", col=2, lwd=1, pch=2)
  plot_labels[5] <- paste("Std. Deviation")
  lines(c(1,51,101), unlist(lower), type="b", col=2, lwd=1, pch=2)
  plot_labels[6] <- paste("Std. Deviation")
  polygon(c(c(1,51,101), rev(c(1,51,101))), c(upper, rev(lower)), col = adjustcolor("red",alpha.f=0.2) )
  legend("topright",plot_labels, lwd=c(1), col=c(4,2,2,3,2,2), pch=c(3,4,4,1,2,2), y.intersp=1)
  header <- paste("Accuracy, ",description, sep="")
  title(header)
  plot(c(1,51,101), unlist(runtimeList), type="b", col=1, lwd=1, pch=1, xlab="K value", ylab="Mean Runtime [s]")
  header <- paste("Runtime, ",description, sep="")
  title(header)
}


#Raw, all persons in, 10 different k vals
knn_cv_and_plot(dataset_shuffle, "raw data - all persons in") #### WARNING: Roughly 11 hour runtime

#Raw, disjunct, 10 different k vals
knn_cv_and_plot(id, "raw data - disjunct", TRUE) #### WARNING: Roughly 11 hour runtime #TODO

#Raw, all persons in and disjunct, 3 different k vals (made for overfit check, be sure to adjust functions to do this)
knn_cv_and_plot_small_run(dataset_shuffle, "raw data - all persons in - overfit check") #### WARNING: Roughly 25 hour runtime
knn_cv_and_plot_small_run(id, "raw data - disjunct - overfit check", TRUE) #### WARNING: Roughly 25 hour runtime #TODO


#Gau smoothing, 3 different k vals, all-persons-in 
gu_si_075.all <- gaussianSmoothing(dataset_shuffle[,-1], 0.75)
gu_si_075.all <- cbind(V1 = dataset_shuffle[1], gu_si_075.all)
knn_cv_and_plot_small_run(gu_si_075.all, "gaussian smoothing applied, sigma = 0.75, all persons in")

#Gau smoothing, 3 different k vals, disjunct #TODO
gu_si_075.dis <- gaussianSmoothing(id[,-1], 0.75)
gu_si_075.dis <- cbind(V1 = id[1], gu_si_075.dis)
knn_cv_and_plot_small_run(gu_si_075.dis, "gaussian smoothing applied, sigma = 0.75, disjunct", TRUE)

#Z-score standardization, 3 different k vals, all persons in 
id_z_normalized.all <- as.data.frame(scale(dataset_shuffle[-1]))
id_z_normalized.all <- cbind(V1 = dataset_shuffle[1], id_z_normalized.all)
knn_cv_and_plot_small_run(id_z_normalized.all, "Z-score normalization applied, all persons in")

#Z-score standardization, 3 different k vals, disjunct #TODO
id_z_normalized.dis <- as.data.frame(scale(id[-1]))
id_z_normalized.dis <- cbind(V1 = id[1], id_z_normalized.dis)
knn_cv_and_plot_small_run(id_z_normalized.dis, "Z-score normalization applied, disjunct", TRUE)



#PCA on raw, all persons in data #TODO
knn_cv_and_plot_pca(dataset_shuffle, "raw data - all persons in, 10 PCs", 10)
knn_cv_and_plot_pca(dataset_shuffle, "raw data - all persons in, 21 PCs", 21)
knn_cv_and_plot_pca(dataset_shuffle, "raw data - all persons in, 31 PCs", 31)


#PCA on raw, disjunct data #TODO
knn_cv_and_plot_pca(id, "raw data - disjunct, 10 PCs", 10, TRUE)
knn_cv_and_plot_pca(id, "raw data - disjunct, 21 PCs", 21, TRUE)
knn_cv_and_plot_pca(id, "raw data - disjunct, 31 PCs", 31, TRUE)


##################################
### Combined preprocessing w/ PCA
##################################

#Min-max -> Z-score -> Gau (.75)-> PCA 21 - all persons in #TODO
id_min_max_normalized <- as.data.frame(lapply(dataset_shuffle[-1], normalize))
id_min_max_z_normalized <- as.data.frame(id_min_max_normalized)
min_max_z_gau <- gaussianSmoothing(id_min_max_z_normalized, 0.75)
min_max_z_gau <- cbind(V1 = dataset_shuffle[1], min_max_z_gau)
knn_cv_and_plot_pca(min_max_z_gau, "Min-max -> Z -> Gau (.75) - all persons in, 21 PCs", 21)



#Min-max -> Z-score -> Gau -> PCA 21 - disjunct #TODO
id_min_max_normalized <- as.data.frame(lapply(id[-1], normalize))
id_min_max_z_normalized <- as.data.frame(id_min_max_normalized)
min_max_z_gau <- gaussianSmoothing(id_min_max_z_normalized, 0.75)
min_max_z_gau <- cbind(V1 = id[1], min_max_z_gau)
knn_cv_and_plot_pca(min_max_z_gau, "Min-max -> Z -> Gau (.75) - disjunct, 21 PCs", 21, TRUE)

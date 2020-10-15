#########################################
#                                       #
# CODE WRITTEN BY FLORIANNE VERKROOST   #
#                                       #
# PLEASE REFER TO REPOSITORY LICENSE    #
# FOR PERMISSION AND COPYRIGHT NOTICES  #
#                                       #
#########################################

set.seed(123)

# ------------------------------------------------------------------------------
# Load necessary libraries
# ------------------------------------------------------------------------------

library(png)
library(fpc)
library(tree)
library(e1071)
library(ROCR)
library(randomForest)
library(kernlab)
library(devtools)
library(keras)
install_keras()

# ------------------------------------------------------------------------------
# Read data into R
# ------------------------------------------------------------------------------

mnist <- keras::dataset_mnist()
train.label = mnist$train$y
test.label = mnist$test$y
train.data = mnist$train$x
test.data = mnist$test$x

# ------------------------------------------------------------------------------
# Create function to construct images of digits
# ------------------------------------------------------------------------------

train.label <- as.factor(train.label)
test.label <- as.factor(test.label)

first10.label <- train.label[1:10]
first10.data <- train.data[1:10,]

createImage <- function(data){
  mat2 <- matrix(nrow=28)
  for (i in 1:5){
    row <- unlist(data[i,])
    mat1 <- matrix(1-row/256, nrow=28)
    mat2 <- cbind(mat2, t(mat1))
  }
  mat3 <- matrix(nrow=28)
  for (i in 6:10){
    row <- unlist(data[i,])
    mat1 <- matrix(1-row/256, nrow=28)
    mat3 <- cbind(mat3, t(mat1))
  }
  writePNG(rbind(mat2[,-1], mat3[,-1]), "digits.png")
}

createImage(first10.data)

# ------------------------------------------------------------------------------
# K-means Clustering
# ------------------------------------------------------------------------------

# Function for three measures of tuning
# Calinski Harabasz (CH), Average silhouette width (ASW), Within sum of squares (SSE)
# Adapted from Michel van der Velden

tuneK <- function(x, range_k, crit = c("within.cluster.ss", "avg.silwidth", "ch"), n = 100){
  
  crit_out <- NULL
  
  if (range_k[1] == 1){ range_k <- range_k + 1 }
  for (i in range_k){
    out <- kmeans(x, i, nstart = n, iter.max = 20)
    clus_crit <- cluster.stats(dist(x), out$cluster)
    crit_out <- rbind(crit_out, clus_crit[crit])
  }
  
  plot(range_k, crit_out, type = "b", ylab = crit)
  k_best <- range_k[which.max(crit_out)]
  
  return(k_best)
  
}

# Compute three tuning measures for different values of K
# Plots sometimes indicated that larger K was necessary

ind = sample(1:nrow(train.data), 1000)
Kch1 <- tuneK(train.data[ind,], 1, meas="ch")
Kasw1 <- tuneK(train.data[ind,], 1, meas="asw")
Ksse1 <- tuneK(train.data[ind,], 1, meas="sse")
Kch5 <- tuneK(train.data[ind,], 1:5, meas="ch")
Kasw5 <- tuneK(train.data[ind,], 1:5, meas="asw")
Ksse5 <- tuneK(train.data[ind,], 1:5, meas="sse")
Kch10 <- tuneK(train.data[ind,], 1:10, meas="ch")
Kasw10 <- tuneK(train.data[ind,], 1:10, meas="asw")
Ksse10 <- tuneK(train.data[ind,], 1:10, meas="sse")
Kch15 <- tuneK(train.data[ind,], 1:15, meas="ch")
Kasw15 <- tuneK(train.data[ind,], 1:15, meas="asw")
Ksse15 <- tuneK(train.data[ind,], 1:15, meas="sse")
Kch20 <- tuneK(train.data[ind,], 1:20, meas="ch")
Kasw20 <- tuneK(train.data[ind,], 1:20, meas="asw")
Ksse20 <- tuneK(train.data[ind,], 1:20, meas="sse")

# Create function that computes K-means clustering
# and automatically computes graphs of the clusters

averages <- function(j, object, k){
  v <- object$cluster
  z <- vector('integer')
  
  # Put all observation rows of one specific cluster in vector z
  for (i in 1:length(v)){
    if (v[i]==j){
      z <- c(z, i)
    }
  }
  
  # Compute matrix m with the data for only those observations
  # in that one cluster and compute column means
  l<-length(z)
  m <- matrix(NA, nrow=l, ncol=784)
  for (i in 1:l){
    m[i,] <- train.data[z[i],]
  }
  cm <- colMeans(m)
  
  # Create images of the cluster
  file_name = paste("plot_cluster", j, k, ".png", sep="-")
  mat2 <- matrix(nrow=28)
  row <- unlist(cm)
  mat1 <- matrix(1-row/256, nrow=28)
  mat2 <- cbind(mat2, t(mat1))
  writePNG(mat2[,-1], file_name)
  
  # Create vector vec that contains train labels to compare
  # train labels of observations in a cluster with the
  # graph of that cluster
  vec <- rep(0, l)
  for (i in 1:l){
    vec[i] <- as.numeric(train.label[z[i]])-1
  }
  return(vec)
}


# For K = 1, 2, 5, 10, 15, 20
# Compute K-means for that number of clusters K
# Graph all obtained clusters and compare to training labels

k <- 1
clust.out.1 <- kmeans(train.data, k, nstart=100, iter.max=20)
for (i in 1:k){
  print(averages(i, clust.out.1, k))
}

k <- 2
clust.out.2 <- kmeans(train.data, k, nstart=100, iter.max=20)
for (i in 1:k){
  print(averages(i, clust.out.2, k))
}

k <- 5
clust.out.5 <- kmeans(train.data, k, nstart=100, iter.max=20)
for (i in 1:k){
  print(averages(i, clust.out.5, k))
}

k <- 10
clust.out.10 <- kmeans(train.data, k, nstart=100, iter.max=20)
for (i in 1:k){
  print(averages(i, clust.out.10, k))
}

k <- 15
clust.out.15 <- kmeans(train.data, k, nstart=100, iter.max=20)
for (i in 1:k){
  print(averages(i, clust.out.15, k))
}

k <- 20
clust.out.20 <- kmeans(train.data, k, nstart=100, iter.max=20)
for (i in 1:k){
  print(averages(i, clust.out.20, k))
}

# ------------------------------------------------------------------------------
# Single Classification Tree
# ------------------------------------------------------------------------------

# Create tree
tree <- tree(label~., data=train.frame)
summary(tree)

# Plot tree
plot(tree)
text(tree, pretty=0)

# Train error
pred.train <- predict(tree, train.frame, type="class")
conf.train <- table(pred.train, train.frame[,1])
error.train <- 1-sum(diag(conf.train))/nrow(train.frame)

# Test error
pred.test <- predict(tree, test.frame, type="class")
conf.test <- table(pred.test, test.frame[,1])
error.test <- 1-sum(diag(conf.test))/nrow(test.frame)

# ------------------------------------------------------------------------------
# Random Forest
# ------------------------------------------------------------------------------

# Tune to get optimal RF parameters (mtry)
tuneRF(train.data, train.label, doBest=T)

# Compute random forest for optimal parameters from tuning
# Give ntree, mtry, error & confusion matrix for train (OOB) and test set
rf.all <- randomForest(x=train.data, y=train.label,  xtest=test.data, ytest=test.label, mtry=56, importance=TRUE)
rf.all

# Plot error vs. ntree and variable importance
plot(rf.all)
varImpPlot(rf.all)
imp.acc <- sort(rf.all$importance[,11], decreasing=TRUE)
imp.gini <- sort(rf.all$importance[,12], decreasing=TRUE)

# Compute predictions
rf.all$test$predicted[1:15]
test.label[1:15]

# Compute confusion matrix and error
conf.test <- table(test.label, rf.all$test$predicted)
err.test <- 1-sum(diag(conf.test))/nrow(test.total)


# ------------------------------------------------------------------------------
# SVM subset of data (1,000 training observations and 167 test observations)
# ------------------------------------------------------------------------------

# Create subsamples of training data (to get 1/60th of original total oftraining observations)
train.total <- cbind(as.data.frame(train.label), as.data.frame(train.data))
colnames(train.total)[colnames(train.total)=="train.label"] <- "label"
train.frame <- as.data.frame(train.total)
train.frame[,1] <- as.factor(train.frame[,1])
train.row <- nrow(train.total)/60
train.sample <- train.frame[sample(nrow(train.frame), train.row, replace=FALSE), ]

# Create subsamples of test data (to get 1/60th of original total of test observations)
test.total <- cbind(as.data.frame(test.label), as.data.frame(test.data))
colnames(test.total)[colnames(test.total)=="test.label"] <- "label"
test.frame <- as.data.frame(test.total)
test.frame[,1] <- as.factor(test.frame[,1])
test.row <- round(nrow(test.total)/60)
test.sample <- test.frame[sample(nrow(test.frame), test.row, replace=FALSE), ]

# Tune for finding optimal parameters for linear kernel
tune.lin <- tune(svm, label ~., data=train.sample, kernel="linear", # radial is default
                 ranges=list(cost=c(.1, 1, 10, 100, 1000),
                             gamma=c(.5, 1, 2, 3, 4, 5)))
summary(tune.lin)

# Tune for finding optimal parameters for radial kernel
tune.1000 <- tune(svm, label ~., data=train.sample, kernel="radial", # radial is default
                  ranges=list(cost=c(.1, 1, 10, 100, 1000),
                              gamma=c(.5, 1, 2, 3, 4, 5)))
summary(tune.1000)

# Compute linear SVM and confusion matrix plus error for both training and test predictions
svm1000lin <- svm(label ~., data=train.sample, kernel="linear", gamma=.5, cost=.1, decision.values=TRUE)
tabletrain.lin <- table(true=train.sample[,1], pred=predict(svm1000lin, train.sample[,-1]))
errortrain.lin <- 1-sum(diag(tabletrain.lin))/sum(tabletrain.lin)
tabletest.lin <- table(true=test.sample[,1], pred=predict(svm1000lin, test.sample[,-1]))
errortest.lin <- 1-sum(diag(tabletest.lin))/sum(tabletest.lin)

# Compute radial SVM and confusion matrix plus error for both training and test predictions
svm1000rad <- svm(label ~., data=train.sample, kernel="radial", gamma=.5, cost=.1, decision.values=TRUE)
tabletrain.rad <- table(true=train.sample[,1], pred=predict(svm1000rad, train.sample[,-1]))
errortrain.rad <- 1-sum(diag(tabletrain.rad))/sum(tabletrain.rad)
tabletest.rad <- table(true=test.sample[,1], pred=predict(svm1000rad, test.sample[,-1]))
errortest.rad <- 1-sum(diag(tabletest.rad))/sum(tabletest.rad)

# Compute radial SVM and confusion matrix plus error for both training and test predictions
# Vary gamma here (so X=1/nvar, 0.5, 1, 2, 3, 4, etcetera)
svm1000flex <- svm(label~., data=train.sample, kernel="radial", gamma=X, cost=.1, decision.values=TRUE)
tabletrain.flex <- table(true=train.sample[,1], pred=predict(svm1000flex, train.sample[,-1]))
errortrain.flex <- 1-sum(diag(tabletrain.flex))/sum(tabletrain.flex)
tabletest.flex <- table(true=test.sample[,1], pred=predict(svm1000flex, test.sample[,-1]))
errortest.flex <- 1-sum(diag(tabletest.flex))/sum(tabletest.flex)


# ------------------------------------------------------------------------------
# SVM complete data (took too long, no results)
# ------------------------------------------------------------------------------

# Compute linear SVM with simply chosen parameters
svm.lin <- svm(label ~., data=train.frame, kernel="linear", gamma=1, cost=1, decision.values=TRUE) #10000

# Tune to get optimal parameters
tune.out <- tune(svm, label ~., data=train.frame, kernel="radial", # radial is default
                 ranges=list(cost=c(.1, 1, 10, 100, 1000),
                             gamma=c(.5, 1, 2, 3, 4, 5)))
summary(tune.out)

# Compare predicted class labels with true class labels in confusion matrix
true <- test.frame[,1]
pred <- predict(tune.out$best.model, newdata=test.frame[,-1])
table(true, pred)

# Create function to make ROC curve
roc.plot <- function(pred, truth, ...){
  pred.ob <- prediction(pred, truth)
  perf <- performance(pred.ob, "tpr", "fpr")
  plot(perf,...)
}

# Compute radial SVM with tuned parameters
svm.opt <- svm(label ~., data=train.frame, kernel="radial", gamma=.5, cost=.1, decision.values=TRUE)
pred.opt <- predict(svm.opt, train.frame[,-1], decision.values=TRUE)
fit.opt <- attributes(pred.opt)$decision.values

# Create ROC curve (unfortunately not possible for multi-class SVM)
par(mfrow=c(1,2))
roc.plot(fit.opt, train.frame[,1], main="Training Data")

# Compute radial SVM and vary gamma here
svm.flex <- sm(label~., data=train.frame, kernel="radial", gamma=3, cost=.1, decision.values=TRUE)
fit <- attributes(predict(svm.flex, train.frame, decision.values=TRUE))$decision.values
roc.plot(fit, train.label, add=TRUE, color="red")

# Try Gaussian kernel
svm.gauss <- ksvm(label ~., data=train.sample, type="C-svc",kernel='rbf',kpar=list(sigma=1),C=1)

# Try polynomial kernel (vary degree (e.g. 4 and 5))
tune.poly <- tune(svm, label ~., data=train.sample, kernel="polynomial", degree=5, # radial is default
                  ranges=list(cost=c(.1, 1, 10, 100, 1000),
                              gamma=c(.5, 1, 2, 3, 4, 5)))

conf.train.poly <- table(train.sample[,1], predict(tune.poly$best.model, train.sample[,-1]))
err.train.poly <- 1-(sum(diag(conf.train.poly))/sum(conf.train.poly))
conf.test.poly <- table(test.sample[,1], predict(tune.poly$best.model, test.sample[,-1]))
err.test.poly <- 1-(sum(diag(conf.test.poly))/sum(conf.test.poly))

# ------------------------------------------------------------------------------
# SVM binary two-class (took too long, no results)
# ------------------------------------------------------------------------------

# Create subsamples of training data for only labels 4 and 9
train.sub <- subset(train.frame, train.frame$label==4 | train.frame$label==9)
train.sub[,1]<-as.factor(as.character(train.sub[,1]))

# Create subsamples of test data for only labels 4 and 9
test.sub <- subset(test.frame, test.frame$label==4 | test.frame$label==9)
test.sub[,1]<-as.factor(as.character(test.sub[,1]))

# Compute linear SVM tune to get optimal parameters
tune.sub.lin <- tune(svm, label ~., data=train.sub, kernel="linear", # radial is default
                     ranges=list(cost=c(.1, 1, 10, 100, 1000),
                                 gamma=c(.5, 1, 2, 3, 4, 5)))
summary(tune.out)

# Compare best linear SVM predicted class labels with true labels
true.sub.lin <- test.sub[,1]
pred.sub.lin <- predict(tune.sub.lin$best.model, newdata=test.sub[,-1])
table.sub.lin <- table(true.sub.lin, pred.sub.lin)

# Compute radial SVM tune to get optimal parameters
tune.sub.rad <- tune(svm, label ~., data=train.sub, kernel="radial", # radial is default
                     ranges=list(cost=c(.1, 1, 10, 100, 1000),
                                 gamma=c(.5, 1, 2, 3, 4, 5)))
summary(tune.out)

# Compare best radial SVM predicted class labels with true labels
true.sub.rad <- test.sub[,1]
pred.sub.rad <- predict(tune.sub.rad$best.model, newdata=test.sub[,-1])
table.sub.rad <- table(true.sub.rad, pred.sub.rad)

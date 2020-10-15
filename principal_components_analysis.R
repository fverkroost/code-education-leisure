#########################################
#                                       #
# CODE WRITTEN BY FLORIANNE VERKROOST   #
#                                       #
# PLEASE REFER TO REPOSITORY LICENSE    #
# FOR PERMISSION AND COPYRIGHT NOTICES  #
#                                       #
#########################################

library(ggplot2) # load ggplot2 package into R
library(stats) # load ggplot2 package into R
library(ggrepel) # load ggplot2 package into R

# Load data from the Places Rated Almanac,
# by Richard Boyer and David Savageau,
# copyrighted and published by Rand McNally
data_raw <- read.table("http://www.stat.nthu.edu.tw/~swcheng/Teaching/stat5191/assignment/places.txt") # load data from online .txt file
delete_vars <- c("CaseNum", "Long", "Lat", "Pop", "StNum") # define irrelevant variables to be deleted
data_raw <- data_raw[, !names(data_raw) %in% delete_vars] # delete irrelevant variables
colnames(data_raw) <- c("Climate", "HousingCost", "HealthCare", "Crime", "Transport", "Education", "Arts", "Recreation", "Economy") # give neater names to variables
X <- scale(as.matrix(data_raw), center = TRUE, scale = TRUE)  # center and scale the rawdata matrix
n <- nrow(X) # n is the number of observations
m <- ncol(X) # m is the number of variables

# Method 1: Program PCA manually using SVD
pca.fn <- function(X){
  s <- svd(X, nu = 0) # singular value decomposition on X
  eigenvalues1 <- s$d^2/(n - 1) # eigenvalues related to biased estimate of variance
  standard_deviations1 <- sqrt(eigenvalues1) # standard deviations
  eigenvectors1 <- s$v # eigenvectors
  scores1 <- X %*% eigenvectors1 # scores/component loadings
  prop_var1 <- s$d^2/sum(s$d^2) # proportion of VAF
  total_var1 <- sum(diag(cov(X))) # total VAF
  cum_var1<-rep(NA,ncol(X)) # empty vector for cumulative VAF (to be filled)
  for(i in 1:m){cum_var1[i]<-sum(sum(prop_var1[1:i]))} # fill cumulative VAF vector
  return(list(eigenvalues1, standard_deviations1, eigenvectors1, scores1, prop_var1, cum_var1, total_var1)) # return list with output
}
res.own <- pca.fn(X) # run the pca function for X

# Scree plot
plot_data <- data.frame(NA, m, 2) # create empty data frame (to be filled)
for (i in 1:m){plot_data[i,1] <- sort(unlist(res.own[1]), decreasing=TRUE)[i]} # fill column 1 with decreasing eigenvalues
for (i in 1:m){plot_data[i,2] <- seq(from = 1, to = m, by = 1)[i]} # fill column 2 with component numbers
colnames(plot_data) <- c("Eigenvalue", "ComponentNumber") # assign column names
ggplot(plot_data, aes(x=ComponentNumber, y=Eigenvalue)) + geom_line() + 
  geom_point(shape = "o", size=3) + scale_shape(solid = FALSE) + ggtitle("PCA Scree Plot") + 
  xlab("Component Number") + ylab("Eigenvalue") +
  theme_bw() + theme(plot.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,max(unlist(res.own[1]))+1), breaks=seq(0,max(unlist(res.own[1]))+1,0.5)) +
  scale_x_continuous(expand = c(0, 0), limits=c(0,m+1), breaks=seq(0,m+1,2))

# Biplot
biplot_data <- data.frame(NA, m, 3) # create empty dataframe
for (i in 1:m){biplot_data[i,1] <- t(colnames(data_raw))[i]} # fill column 1 with variable names
for (i in 1:m){biplot_data[i,2] <- unlist(res.own[3])[i]} # fill column 2 with eigenvectors for first component
for (i in 1:m){biplot_data[i,3] <- unlist(res.own[3])[(i+9)]} # fill column 2 with eigenvectors for second component
colnames(biplot_data) <- c("Name", "PrincipalComponent1", "PrincipalComponent2") # assign column names
zerovec <- vector(mode="numeric", length=m) # create zero vector

# Plot 1 for variables
biplot1 <- ggplot(biplot_data, aes(x=PrincipalComponent1, y=PrincipalComponent2, colour="FF0000", label=Name)) + 
  coord_fixed(ratio = 1) +
  geom_point() + guides(fill=FALSE) + guides(colour=FALSE) +
  geom_label_repel(aes(label=Name, fill="FF0000"), size=2,
                   fontface = 'bold', color = 'white',
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines")) +
  scale_shape(solid = FALSE) + ggtitle("PCA Biplot") + 
  xlab("Principal Component 1") + ylab("Principal Component 2") +
  theme_bw() + theme(plot.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()) +
  scale_y_continuous(limits=c(min(biplot_data[,3])-.5,max(biplot_data[,3])+.5), breaks=seq(round(min(biplot_data[,3])-.5),round(max(biplot_data[,3])+.5), by=.5)) +
  scale_x_continuous(limits=c(min(biplot_data[,2])-.5,max(biplot_data[,2])+.5), breaks=seq(round(min(biplot_data[,2])-.5),round(max(biplot_data[,2])+.5), by=.5)) +
  geom_segment(data=biplot_data, mapping=aes(x=zerovec, y=zerovec, xend=biplot_data[,2], yend=biplot_data[,3]), size=.2, color="blue") +
  theme(plot.background = element_rect(fill = "white"))

# Plot 2 for scores
test_data <- data.frame(seq(1,n,1), unlist(res.own[4])[1:n], unlist(res.own[4])[(n+1):(2*n)]) # create data used for biplot2
biplot2 <- ggplot(test_data, aes(x=test_data[,2], y=test_data[,3])) + geom_point(alpha=.25, size=1) # show objects as points
biplot2 <- ggplot(test_data, aes_string(x=test_data[,2], y=test_data[,3])) + geom_text(alpha=.1, size=3, aes(label=rownames(X))) # use this if preference for objects as names
biplot2 <- biplot2 + geom_hline(aes(yintercept=0), size=.2) + geom_vline(aes(xintercept=0), size=.2) +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(panel.background = element_rect(fill = NA))

# Bootstrap on eigenvalues and component loadings to determine stability and standard deviations
library("boot")                       # load the boot package into R
boot.fn.eig <- function(data, index){ # make a function that returns the 
  res <- pca.fn(X[index,])            # pca eigenvalues for data selected
  return(unlist(res[1]))              # by the vector index
}
res.boot.eig <- boot(X, boot.fn.eig, R = 10000)  # run 10000 bootstraps
print(res.boot.eig, digits = 3)                  # print bootstrap results
plot(res.boot.eig, index =  1)                   # histogram of 1st eigenvalue of the bootstraps
plot(res.boot.eig, index =  2)                   # histogram of 2nd eigenvalue of the bootstraps
boot.ci(res.boot.eig, index = 1, type = "perc")  # show percentile type of 95% confidence intervals 
boot.fn.load <- function(data, index){  # make a function that returns the 
  res <- pca.fn(X[index,])              # pca component loadings for data selected
  return(unlist(res[5]))                # by the vector index
}
res.boot.load <- boot(X, boot.fn.load, R = 10000) # run 10000 bootstraps
print(res.boot.load, digits = 3)                  # print bootstrap results
plot(res.boot.load, index =  1)                   # histogram of 1st component loading of the bootstraps
plot(res.boot.load, index =  2)                   # histogram of 2nd component of the bootstraps
boot.ci(res.boot.load, index = 1, type = "perc")  # show percentile type of 95% confidence intervals

X.rec <- svd(X)$u %*% diag(svd(X)$d) %*% t(svd(X)$v) # reconstruct matrix X

# Method 2: Use standard function prcomp
res.prcomp <- prcomp(X, cor=TRUE)
standard_deviations3 <- res.prcomp$sdev
eigenvalues3 <- standard_deviations3^2 
eigenvectors3 <- res.prcomp$rotation
scores3 <- res.prcomp$x
summary(res.prcomp)
screeplot(res.prcomp, type="lines")

# Bootstrap via prcomp on eigenvalues and component loadings to determine stability and standard deviations
boot.prcomp.eig <- function(data, index){   # make a function that returns the 
  res <- prcomp(X[index,], cor = TRUE)      # pca eigenvalues for data selected
  return(res$sdev^2)                        # by the vector index
}
boot.prcomp.eig <- boot(X, boot.prcomp.eig, R = 10000) # run 10000 bootstraps
print(boot.prcomp.eig, digits = 3)                     # print bootstrap results
plot(boot.prcomp.eig, index =  1)                      # histogram of 1st eigenvalue of the bootstraps
plot(boot.prcomp.eig, index =  2)                      # histogram of 2nd eigenvalue of the bootstraps
boot.ci(boot.prcomp.eig, index = 1, type = "perc")     # show percentile type of 95% confidence intervals
boot.prcomp.load <- function(data, index){    # make a function that returns the 
  res <- prcomp(X[index,], cor = TRUE)        # pca component loadings for data selected
  return(res$x)                               # by the vector index
}
boot.prcomp.load <- boot(X, boot.prcomp.load, R = 10000) # run 10000 bootstraps
print(boot.prcomp.load, digits = 3)                      # print bootstrap results
plot(boot.prcomp.load, index =  1)                       # histogram of 1st component loading of the bootstraps
plot(boot.prcomp.load, index =  2)                       # histogram of 2nd component of the bootstraps
boot.ci(boot.prcomp.load, index = 1, type = "perc")      # show percentile type of 95% confidence intervals

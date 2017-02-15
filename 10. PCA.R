# Chapter 10 Lab 
rm(list=ls())
# -------------------
# 1: Principal Components Analysis
# This data set contains statistics, in arrests per 100,000 residents for assault, murder, and rape in 
# each of the 50 US states in 1973. Also given is the percent of the population living in urban areas.

states <- row.names(USArrests)
names(USArrests) # variable names
apply(USArrests, 2, mean)  # average arrest for each variable
apply(USArrests, 2, var)  # s.dev of arrests for each variable
pr.out <- prcomp(USArrests, scale = TRUE)  # perform PCA and standardise variables to have mean = 0 and SD = 1

# view output
pr.out$center  # the centering used (the mean in this case)
pr.out$scale  # the scale used (the SDev in this case)
pr.out$rotation  # loadings matrix of factors onto components (columns are eigenvectors)
pr.out$x  # centred and scaled data multiplied by loadings matrix i.e. principal component score vectors
dim(pr.out$x)

# plot plot the first two principal components 
biplot(pr.out, scale = 0, cex = 0.6) # arrows are scaled to represent the loadings

# tweak plot as PCs only unique up to a sign change
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0, cex = 0.6)

# cont. to variance
pr.out$sdev  # standard deviation of each principal component
pr.var <- pr.out$sdev^2  # variance of each principal component
pr.var
pve <- pr.var/sum(pr.var)  # variance explained of each PC
pve

# Plot charts
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = 'b')

# ---------------------
# 2: Clustering

# 2a. K-Means Clustering

# Create data clusters
set.seed(2)
x <- matrix(rnorm(50*2), ncol=2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4

# perform a k means with 2 clusters and plot
km.out <- kmeans(x, 2, nstart = 20)  # use 20 initial random cluster assignments
km.out$cluster  # contains cluster assignments for each observation
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K = 2", xlab = "", ylab = "", pch = 20, cex = 2)

# performa k means with 3 clusters and plot (as ex-ante we won't know number of clusters (though here we do as we made data up))
set.seed(4)
km.out <- kmeans(x, 3, nstart = 20)  # use 20 initial random cluster assignments
km.out
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K = 3", xlab = "", ylab = "", pch = 20, cex = 2)

# compare 1 random start to 20 (kmeans only reports best)
# withinss: vector of within-cluster sum of squares, one component per cluster
# tot.withinss: Total within-cluster sum of squares
set.seed(3)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss

# 2b. Hierarchical Clustering
# compare 3 of the methods using euclidean distance
hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")
par(mfrow = c(1,3))
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = .9)

# compare to different package
hc.agnes <- agnes(x)
# plot(hc.agnes)

# View cluster labels for each observation associated with a given cut of the dendrogram
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

# scale  variables before performing hierarchical clustering, use the scale() function
xsc <- scale(x)
par(mfrow = c(1,1))
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical Clustering with Scaled Features")

# create new data with 3 features
par(mfrow = c(1,2))
x <- matrix(rnorm(30*3), ncol = 3)
# c <- cor(t(x)) # correlation of the observations to each other, not the features(!)
# t1 <- 1 - c
dd <- as.dist(1 - cor(t(x)))  # convert to format hclust can read: same results if just use as.dist(cor(t(x)))
plot(hclust(dd, method = "complete"), main = "Complete Linkage with Correlation-Based Distance", xlab = "", sub = "")

# test of ISLR comment: the absolute correlation between any two observations with measurements on two features is always 1
x1 <- matrix(rnorm(30*2), ncol = 2)
tx1 <- t(x1)
c1 <- cor(tx1)

# --------------------
# 3a. NCI60 Data Example

# NCI microarray data, containing expression levels on 6,830 genes from 64 cancer cell lines. Cancer type is also recorded.
# data is a 64 by 6,830 matrix of the expression values while labs is a vector listing the cancer types for the 64 cell lines.
library(ISLR)
nci.labs <- NCI60$labs  # labels of cancer type
nci.data <- NCI60$data  # raw data
dim(nci.data)
table(nci.labs) # summary of cancer types

# PCA on the NCI60 Data

pr.out = prcomp(nci.data, scale = TRUE)  # perform PCA (we have scaled, though one could argue that it is better not to scale the genes)

Cols = function(vec){
    # simple function that assigns a distinct color to each element of a numeric vector. 
    # The function will be used to assign a color to each of the cell lines, based on the 
    # cancer type to which it corresponds
    cols = rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}

# Projections of the NCI60 cancer cell lines onto the first 3 PCs (in other words, scores for the first 3 PCs)
# On the whole, observations belonging to a single cancer type tend to lie near each other in this low-dimensional space
par(mfrow = c(1,2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")  # PC score vectors 1 and 2
plot(pr.out$x[, c(1,3)], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z3")  # PC score vectors 1 and 2

# summary of the proportion of variance explained (plots)
summary(pr.out)
plot(pr.out)  # scree plot
pve = 100*pr.out$sdev^2/sum(pr.out$sdev^2)  # proportion of variance explained
par(mfrow = c(1,2))
plot(pve,  type = "o", ylab = "PVE", xlab = "Principal Component", col = "blue") 
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component", col = "brown3")

# ---------------
# 3b. Hierarchically clustering the observations of the NCI60 Data

# use the 3 types of hierarchical clustering

# NOte: single linkage will tend to yield trailing clusters (very large clusters onto which individual observations
# attach one-by-one). On the other hand, complete and average linkage tend to yield more balanced, attractive clusters
sd.data = scale(nci.data)  # scale data
par(mfrow = c(3, 1))
data.dist = dist(sd.data)  # Euclidean distance is used as the dissimilarity measure
plot(hclust(data.dist), labels = nci.labs, main = "Complete Linkage", xlab = "", sub = "",ylab = "")
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linkage", xlab = "", sub = "",ylab = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs,  main = "Single Linkage", xlab = "", sub = "",ylab = "")

# cut the dendrogram at the height that will yield a particular number of clusters, say four
# plot results and show where cut would be
# Note: this classifies each cancer cell line based on the full 6,830 gene expressions
hc.out = hclust(dist(sd.data))
hc.clusters = cutree(hc.out, 4)
table(hc.clusters, nci.labs) # table of how many cancer types in each cluster
par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red") # argument h = 139 plots a horizontal line at height 139 on the dendrogram, same as cutree(hc.out,4)
hc.out

# Compare to what we get if we perform K-means clustering with K = 4
set.seed(2)
km.out = kmeans(sd.data, 4, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)

# can perform hierarchical clustering on the first 5 PCs
# Note: this classifies each cancer cell line based on just the first 5 PCs
first5PCs <- pr.out$x[, 1:5]
distFirst5PCs <- dist(first5PCs)  # size 64 x 64
hc.out.PC = hclust(distFirst5PCs)
plot(hc.out.PC, labels = nci.labs, main = "Hier. Clust. on First Five PC Score Vectors")
hc.clusters.PC <- cutree(hc.out.PC, 4)

# compare results
table(hc.clusters, nci.labs)  # compare to actual labels
table(hc.clusters, hc.clusters.PC)  # compare to full data hierarchical clusters
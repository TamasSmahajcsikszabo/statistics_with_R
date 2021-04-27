protein <- read.table("../data/PDSwR2/Protein/protein.txt", sep = "\t", header = TRUE)
vars <- colnames(protein)[-1]
to_scale <- protein[, vars]

scaled <- scale(to_scale)
d <- dist(scaled, method = "euclidean")
cluster <- hclust(d, method = "ward")
plot(cluster, labels = protein$Country)
rect.hclust(cluster, k = 5)

groups <- cutree(cluster, h = 5)

print_clusters <- function(data, labels, h) {
  for (i in 1:h) {
    print(data[labels == i, ])
  }
}

print_clusters(protein, groups, 5)

# projecting the data on the first two principal components
library(ggplot2)
pca <- prcomp(scaled)
ncomp <- 2
project <- predict(pca, scaled)[, 1:ncomp]

project.plus <- cbind(as.data.frame(project), cluster = as.factor(groups), county = protein$Country)
ggplot(project.plus, aes(PC1, PC2, color = cluster)) +
  geom_point() +
  geom_text(aes(label = county), vjust = 1)

# evaluating clusters
library(fpc)
hclustCBI
kbest.p <- 5
cboot.clust <- clusterboot(scaled, clustermethod = hclustCBI, method = "ward", k = kbest.p)
summary(cboot.clust$result)
groups <- cboot.clust$result$partition
cboot.clust$bootmean
cboot.clust$bootbrd

# choosing the number for k

# 1. total within sum of squares (tWSS)
# WSS for a cluster is the average squared distance of each point from the centroid

sqrt_edist <- function(x, y) {
  # x and y are vectors
  sum((x - y)^2)
}

wss.cluster <- function(clustermatrix) {
  c0 <- apply(clustermatrix, 2, FUN = mean)
  sum(apply(clustermatrix, 1, FUN = function(row) {
    sqrt_edist(row, c0)
  }))
}

wss.total <- function(dmatrix, labels) {
  wsstot <- 0
  k <- length(unique(labels))
  for (i in 1:k) {
    wsstot <- wsstot + wss.cluster(subset(dmatrix, labels == i))
    # print(paste0(i, ": ", wsstot))
  }
  wsstot
}

wss.total(scaled, labels = groups)
subset(scaled, groups == groups[1])

# 2. Calinski-Harabasz (Ch) index
# ratio of:
# between-cluster variance (the variance of all cluster centroids from the grand centroid)
# and the total within-cluster variance (average WSS of clusters)
# TSS - total sum of squares is the squared distance of all data points from the centroid, independent of the clustering
# if WSS(k) is WSS with k clusters, the between sum of squares BSS(k) = TSS - WSS(k)

grandmean <- function(dmatrix) {
  apply(dmatrix, 2, FUN = mean)
}
grandmean(scaled)

# WSS measures the data point closeness in a cluster
# BSS measures how far apart clusters are from eachother
# good clustering has low WSS and high BSS
# within cluster variance W is WSS(k)/(n - k), where n is the number of points in the cluster
# between cluster variance B is BSS(k) / (k-1)

# as W should decrease with k, and B increase with k, their ratio is maximized with optimal k

# the Ch index:
totss <- function(dmatrix) {
  centroid <- grandmean(dmatrix)
  sum(apply(dmatrix, 1, FUN = function(row) {
    sqrt_edist(row, centroid)
  }))
}

ch_criterion <- function(dmatrix, kmax, method = "kmeans") {
  if (!(method %in% c("hclust", "kmeans"))) {
    stop("method must be one of c('kmeans', 'hclust')")
  }

  ntps <- dim(dmatrix)[1] # number of rows
  totss <- totss(dmatrix)
  wss <- numeric(kmax)
  crit <- numeric(kmax)
  wss[1] <- (ntps - 1) * sum(apply(dmatrix, 2, var)) # WSS with k=1 which is theoretically TSS

  for (k in 2:kmax) {
    if (method == "kmeans") {
      clustering <- kmeans(dmatrix, k, nstart = 10, iter.max = 100)
      wss[k] <- clustering$tot.withinss
    } else {
      d <- dist(dmatrix, method = "euclidean")
      pfit <- hclust(d, method = "ward")
      labels <- cutree(pfit, k = k)
      wss[k] <- wss.total(dmatrix, labels)
    }
  }
  bss <- totss - wss
  crit.num <- bss / (0:(kmax - 1))
  crit.denom <- wss / (ntps - 1:kmax)
  list(crit = crit.num / crit.denom, wss = wss, totss = totss)
}

library(reshape2)
clustcrit <- ch_criterion(scaled, kmax = 10, method = "hclust")
critframe <- data.frame(k = 1:10, ch = scale(clustcrit$crit), wss = scale(clustcrit$wss))
critframe <- melt(critframe, id.vars = c("k"), variable.name = "measure", value.name = "score")

ggplot(critframe, aes(x = k, y = score, color = measure)) +
  geom_point(aes(shape = measure)) +
  geom_line(aes(linetype = measure)) +
  scale_x_continuous(breaks = 1:10, labels = 1:10)


# kmeans
# final clusters depend on the initial clusters
# not guaranteed to have a unique stopping point
# best to run multiple times with other random seeds

plclusters <- kmeans(scaled, kbest.p, nstart = 100, iter.max = 100)
plclusters$size
groups <- plclusters$cluster

# finding the best k:
library(fpc)
clustering.ch <- kmeansruns(scaled, krange = 1:10, criterion = "ch")
clustering.asw <- kmeansruns(scaled, krange = 1:10, criterion = "asw")
critframe <- data.frame(k = 1:10, ch = scale(clustering.ch$crit), asw = scale(clustering.asw$crit))
critframe <- melt(critframe, id.vars = c("k"), variable.name = "measure", value.name = "score")
ggplot(critframe, aes(x = k, y = score, color = measure)) +
  geom_point(aes(shape = measure)) +
  geom_line(aes(linetype = measure)) +
  scale_x_continuous(breaks = 1:10, labels = 1:10)


# clusterboot with kmeans
kbest.p <- 5
cboot <- clusterboot(scaled, clustermethod = kmeansCBI, runs = 100, iter.max = 100, krange = kbest.p, seed = 15555)

# assing new points to clusters
assign_cluster <- function(newpt, centers, xcenter = 0, xscale = 1) {
  xpt <- (newpt - xcenter) / xscale
  dists <- apply(centers, 1, FUN = function(c0) {
    sqrt_edist(c0, xpt)
  })
  which.min(dists)
}


# example of assinging new points to clusters
rnorm.multidim <- function(n, mean, sd, colstr = "x") {
  ndim <- length(mean)
  data <- NULL
  for (i in 1:ndim) {
    col <- rnorm(n, mean = mean[[i]], sd = sd[[i]])
    data <- cbind(data, col)
  }
  cnames <- paste(colstr, 1:ndim, sep = "")
  colnames(data) <- cnames
  data
}

mean1 <- c(1, 1, 1)
sd1 <- c(1, 2, 1)

mean2 <- c(10, -3, 5)
sd2 <- c(2, 1, 2)

mean3 <- c(-5, -5, -5)
sd3 <- c(1.5, 2, 1)

toydata <- data.frame()
for (i in c(1, 2, 3)) {
  assign(paste0("clust", i), rnorm.multidim(100, get(paste0("mean", i)), get(paste0("sd", i))))
  toydata <- rbind(toydata, get(paste0("clust", i)))
}
tmatrix <- scale(toydata)
tcenter <- attr(tmatrix, "scaled:center")
tscale <- attr(tmatrix, "scaled:scale")
kbest.t <- 3
tclusters <- kmeans(tmatrix, kbest.t, 100, 100)


unscale <- function(scaledpt, centervec, scalevec) {
  scaledpt * scalevec / centervec
}
unscale(tclusters$centers[1, ], tcenter, tscale)

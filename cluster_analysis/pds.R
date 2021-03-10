protein <- read.table("data/PDSwR2/Protein/protein.txt", sep="\t", header= TRUE)
vars <- colnames(protein)[-1]
to_scale <- protein[, vars]

scaled <- scale(to_scale)
d <- dist(scaled, method="euclidean")
cluster <- hclust(d, method="ward")
plot(cluster, labels=protein$Country)
rect.hclust(cluster, k=5)

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

project.plus <- cbind(as.data.frame(project), cluster=as.factor(groups), county = protein$Country)
ggplot(project.plus, aes(PC1, PC2, color=cluster)) +
  geom_point() +
  geom_text(aes(label=county), vjust=1)

# evaluating clusters
library(fpc)
kbest.p <- 5
cboot.clust <- clusterboot(scaled, clustermethod = hclustCBI, method="ward", k=kbest.p)
summary(cboot.clust$result)
groups <- cboot.clust$result$partition
cboot.clust$bootmean
cboot.clust$bootbrd

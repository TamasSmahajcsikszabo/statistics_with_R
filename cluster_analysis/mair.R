library(cluster)
library(clusterCrit)
library(NbClust)


# model-based clustering / parametric clustering
# GoF measures, BIC
# probabilistic (soft) assignment to clusters

# these methods are based on mixed distributions:
# mixture components / clusters / latent classes are (normal) distributions

# normal mixture models
# latent profile analysis
library(MPsychoR)
library(mclust)
library(tibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
data("Rmotivation2")
Rwd <- Rmotivation2[, c(4:6)]
set.seed(123)

clustbic <- mclustBIC(Rwd, G = 2:10)
clustfit <- Mclust(Rwd, x = clustbic)
clust_sum <- summary(clustfit, parameters = TRUE) #  note the mixing probabilities are the mixture weights

# the class membership posterior probabilities
head(clustfit$z, 3)
# the hard [deterministic] way:
clustfit$classification # based on the maximum of the posterior probabilities

# describing the clusters
# the mean vectors
clust_sum$mean

# in order to plot the mixture densities, we need PCa and plot them
clustred <- MclustDR(clustfit)
plot(clustred, what = "boundaries", ngrid = 200)
tibble(clustred$dir)
clustred$M
class(clustred)
?plot.MclustDR
pred <- tibble(predict2D.MclustDR(clustred)$uncertainty)
tibble(clustred$dir)


MclustDR_plot <- function(DR_object) {
  class_data <- data.frame(class = DR_object$classification)
  plot_data <- tibble(data.frame(DR_object$dir[, c(1:2)]), class_data)
  l <- nrow(plot_data)
  colnames(plot_data) <- c("x", "y", "class")
  pred <- tibble(data.frame(t(predict2D.MclustDR(DR_object, ngrid = l)$uncertainty)))
  pred <- pred %>%
    pivot_longer(1:l, names_to = "names", values_to = "z") %>%
    dplyr::select(-names)
  x_range <- range(plot_data$x)
  y_range <- range(plot_data$y)
  x_axis <- seq(x_range[[1]], x_range[[2]], length.out = l)
  y_axis <- seq(y_range[[1]], y_range[[2]], length.out = l)
  grid <- tibble(data.frame(expand.grid(x_axis, y_axis)))
  grid <- bind_cols(grid, pred)
  colnames(grid) <- c("x", "y", "z")

  ggplot() +
    geom_tile(data = grid, aes(x, y, alpha = z)) +
    # geom_point(data = plot_data, aes(x, y, shape = class), color = "black", size = 4) +
    geom_point(data = plot_data, aes(x, y, color = class, shape = class), size = 3) +
    scale_color_manual(values = c("coral", "coral3", "cornflowerblue", "seagreen", "gold")) +
    theme_light()
}

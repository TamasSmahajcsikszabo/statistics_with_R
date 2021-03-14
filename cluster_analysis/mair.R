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
plot(clustred, what = "density", dimens = 1)


MclustDR_plot <- function(DR_object) {
  # expects an MclustDR object as input
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
    geom_raster(data = grid, aes(x, y, alpha = z)) +
    # geom_point(data = plot_data, aes(x, y, shape = class), color = "black", size = 4) +
    geom_point(data = plot_data, aes(x, y, color = class, shape = class), size = 3) +
    scale_color_manual(values = c("coral", "coral3", "cornflowerblue", "seagreen", "gold")) +
    theme_light()
}

MclustDR_plot(clustred)

# latent class analysis (LCA)
# categorical input data
# it's a mixture distribution model where the underlying densities are specified using binomial distribution
# for dichotomous, and a multinomial distribution for polytomous items
library(poLCA)
data("AvalanchePrep")
formula <- cbind(info, discuss, gear, decision) ~ 1
set.seed(1)

# training the LCAs
# extract BIC (for LCA minimum BIC applies)
BICs <- c()

# nrep is set to 3 to avoid local maximum, as EM is sensitive to starting values (expectation-maximization)
for (k in 1:4) {
  if (k == 3) {
    customiter <- 2000
  } else if (k == 4) {
    customiter <- 5000
  } else {
    customiter <- 1000
  }
  assign(paste0("fitlca", k), poLCA(formula, data = AvalanchePrep, nclass = k, nrep = 3, maxiter = customiter))
  BICs <- c(BICs, get(paste0("fitlca", k))$bic)
}

# best k
best_k <- seq(1, 4)[BICs == min(BICs)]
get(paste0("fitlca", best_k))
plot(get(paste0("fitlca", best_k)))

# see also:
library(psychomix)

# mixed scale levels
library(tidyverse)
x <- seq(1, 500, by = 1)
data_binom <- tibble(data.frame(x = x, y = dbinom(x, 500, 0.5)))
ggplot(data_binom) +
  geom_point(aes(x, y))


library(flexmix) # a flexible infrastructure to fit all sorts of mixture models

x <- seq(1, 500, 1)
prob <- unlist(lapply(x, function(i) {
  sum(sample(c(0, 1), i, replace = TRUE)) / i
}))
plot(prob)

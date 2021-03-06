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
library(flexmix) # a flexible infrastructure to fit all sorts of mixture models
data("zareki")
set.seed(123)
zarflex <- stepFlexmix(~1,
  data = zareki, k = 1:4, nrep = 3,
  model = list(
    FLXMRmultinom(addit7 ~ .),
    FLXMRmultinom(addit8 ~ .),
    FLXMRmultinom(subtr3 ~ .),
    FLXMRmultinom(subtr7 ~ .),
    FLXMRglm(time ~ ., family = "gaussian") # family can be "gamma" for right-skewed metric variables with 0 lower bound, for counts "poisson"
  )
)

# get the best model based upon BIC
zarflex2 <- getModel(zarflex, "BIC")
cluster <- zarflex2@cluster
table(cluster)

# to get the distribution parameters
pars <- parameters(zarflex2)
# the parameters are on a logit scale, we need exponentiation transformation to get them as odds ratios
catpars <- sapply(pars[1:4], exp)
colnames(catpars) <- c("addit7", "subtr3", "addit8", "subtr7")
# time variable
pars[[5]]

# for mosaic plots:
library(vcd)
vcd::mosaic(catpars)


# the mixture weights are a priori probabilities for individuals to be in component j
# during clustering, these are updated and not influenced by any specific variable
# in mixture models we can specify covariates that influence these weights
# such a covariate w is called concomitant variable
# it has a parameter vector alpha associated with the concomitant variable

zarflexc <- flexmix(~1,
  data = zareki, cluster = posterior(zarflex2),
  concomitant = FLXMRmultinom(~class),
  model = list(
    FLXMRmultinom(addit7 ~ .),
    FLXMRmultinom(addit8 ~ .),
    FLXMRmultinom(subtr3 ~ .),
    FLXMRmultinom(subtr7 ~ .),
    FLXMRglm(time ~ ., family = "gaussian")
    # family can be "gamma" for right-skewed metric variables with 0 lower bound, for counts "poisson"
  )
)
zarflexc@prior # weights with concomitant
zarflex2@prior # weights without concomitant

################################################
## Mixture  Regression #########################

# mixture regression applications
# toy example
set.seed(123)
x <- rnorm(100)
Y1 <- 2 * x + rnorm(100, sd = 0.5)
Y2 <- (-2) * x + rnorm(100, sd = 0.5)
toydat <- data.frame(X = c(x, x), Y = c(Y1, Y2))
lm_fit <- lm(Y ~ X, data = toydat)
coefs <- lm_fit$coefficients
residuals <- lm_fit$residuals
fitted <- lm_fit$fitted.values
resid_plot <- data.frame(x = fitted, y = residuals)

ggplot() +
  geom_point(data = toydat, aes(X, Y), size = 3) +
  geom_abline(aes(slope = coefs[1], intercept = coefs[2]))


# fitting a two-component mixture regresssion
library(flexmix)
toymix <- flexmix(Y ~ X, k = 2, data = toydat)
# parameters are for each component:
# intercept, slope and sigma - residual standard deviation
# clustering is probabilistic which can be turned into hard membership


pars <- data.frame(parameters(toymix))

# real life example of a mixture mixed-effect model (repeated measurement)
# fixed effect is attitude
# random effect is subject
# normally lowest BIC wins out of multiple trials, here we pick K=2

library(MPsychoR)
library(tibble)
data("KoreanSpeech")
tibble(KoreanSpeech)
set.seed(123)
koreamix <- flexmix(frequency ~ attitude | subject, k = 2, data = na.omit(KoreanSpeech))
table(koreamix@cluster)
parameters(koreamix)

# perfect separation by genders
table(Cluster = clusters(koreamix), Gender = na.omit(KoreanSpeech)$gender)


#### GAM mixture
# GAMs are a nonlinear framework which typically uses splines or smoothers to achiveve a nonlinear fit
library(gamair)
library(lattice)
data("brain")

# exclude outliers:
brain <- brain[brain$medFPQ > 5e-3, ]
trellis.par.set(regions = list(col = colorRampPalette(c("cadetblue4", "white", "coral4"))))
levelplot(log(medFPQ) ~ Y * X, data = brain)


# fit the model with K=3


###################################################################################################
# Dirichlet based clustering


# DPM: Dirichlet Process Mixture
# due to Bayesian estimation, number of clusters are directly inferred from the data a. k. a. Chinese Restaurant process
library(dpmixsim)

# 1. dirichlet process regression example
library(profdpm)
koreamix2 <- profLinear(frequency ~ attitude, group = subject, data = na.omit(KoreanSpeech))
# get parameters
koreamix2$m

# perfect separation by genders
table(Cluster = koreamix2$clust, Gender = na.omit(KoreanSpeech)$gender)









#######################################################################
#################  functions ##########################################

prepare_pars <- function(dataset) {
  pars <- data.frame(parameters(dataset))
  row.names(pars) <- c("int", "sl", "sigma")
  pars$param <- row.names(pars)
  pars <- pars %>%
    pivot_longer(1:2, names_to = "comp", values_to = "val")
  pars <- pars %>%
    mutate(cluster = if_else(comp == "Comp.1", "1", "2"))

  pars <- pars %>%
    mutate(cluster = factor(cluster, levels = c(1, 2))) %>%
    dplyr::select(-comp) %>%
    pivot_wider(names_from = param, values_from = val)
  pars
}

plot_mix <- function(dataset, mix, pars, x, y) {
  dataset <- na.omit(dataset)
  ggplot() +
    # geom_point(data = tibble(cbind(dataset, cluster = as.factor(mix@cluster))), aes(as.numeric(x), y, color = cluster)) +
    scale_color_manual(values = c("coral", "cornflowerblue")) +
    geom_abline(data = pars, aes(slope = sl, intercept = int, color = cluster))
}

plot_mix(dataset = KoreanSpeech, mix = koreamix, pars = prepare_pars(koreamix), x = "scenario", y = "frequency")

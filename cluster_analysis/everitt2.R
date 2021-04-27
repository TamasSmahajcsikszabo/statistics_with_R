library(ggplot2)
library(tibble)
library(dplyr)
library(stringr)




dataset <- sample(0.06:1.76, 100, replace = TRUE)

kernel_estimate <- function(x, type = "Gaussian", ...) {
  if (type %in% c("Gaussian", "normal")) {
    K <- function(X) {
      (1 / (sqrt(2 * pi)))^(exp(1)^((-1 / 2) * X^2))
    }
  } else if (type %in% c("rectangular", "rect")) {
    K <- function(X) {
      unlist(lapply(X, function(i) {
        if (abs(i) < 1) {
          1
        } else {
          0
        }
      }))
    }
  } else if (type %in% c("triangular", "tri")) {
    K <- function(X) {
      unlist(lapply(X, function(i) {
        if (abs(i) < 1) {
          1 - abs(i)
        } else {
          0
        }
      }))
    }
  }
  K(x)
}

density_estimate <- function(X, K = "normal", h = 0.005, ...) {
  n <- length(X)
  density_est <- rep(0, n)
  computation <- unlist(lapply(X, function(x) {
    difference_vector <- unlist(lapply(sort(X), function(xi) {
      (x - xi) / h
    }))
    density_est <- kernel_estimate(difference_vector, type = K) + density_est
  }))

  density_est <- density_est / (n * h)
  data.frame(x = sort(X), d = density_est)
}
density_estimate(dataset)


replace <- function(x, o = "", r = "") {
  gsub(o, r, x)
}

density_plot <- function(X, K = "normal", h = 1.28, smoothing = 1.0, ...) {
  density_est <- density_estimate(X, K = K, h = h)
  # binned <- as.character(cut(density_est$x, breaks = floor(length(density_est$x) / (10 * smoothing))))
  # density_est <- bind_cols(density_est, tibble(binned)) %>%
  #   mutate(
  #     lower = as.numeric(str_sub(binned, 2, str_locate(binned, ",")[, 1] - 1)),
  #     upper = as.numeric(str_sub(binned, str_locate(binned, ",")[, 1] + 1, nchar(binned) - 1))
  #   )
  # density_trend <- density_est %>%
  #   rowwise() %>%
  #   mutate(M = median(c(lower, upper), na.rm=TRUE)) %>%
  #   group_by(x = M) %>%
  #   summarise(d = max(d, na.rm=TRUE))
  ggplot() +
    # geom_segment(data = tibble(x = sort(X)), aes(x = x, xend = x, y = 0, yend = 0.05), alpha = 1 / 10) +
    geom_point(data = density_est, aes(x, d)) +
    theme_light()
}
density_plot(X, h = 0.235, smoothing = 6)
plot(density(X))


data(mtcars)
dataset <- mtcars[, c(3, 4)]
bivariate_denisty <- density(as.matrix(scale(dataset)))
plot(bivariate_denisty)

ggplot(dataset) +
  geom_point(aes(disp, hp))

bivariate_density_plot <- function(dataset, title = "", xlab = "", ylabl = "", ...) {
  names(dataset) <- c("x", "y")
  ggplot(dataset, aes(x, y)) +
    geom_point(color = "#e34234") +
    geom_density2d(linetype = "dashed", size = 0.5, color = "#49579F") +
    labs(
      title = title,
      xlab = xlab,
      ylab = ylab
    ) +
    theme_light()
}


bivariate_density_plot(distance[, c(1, 2)])

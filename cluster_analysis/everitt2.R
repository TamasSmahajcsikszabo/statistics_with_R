library(ggplot2)
library(tibble)
library(dplyr)
library(stringr)
library(SMPracticals)




dataset <- seq(-4, 4, length.out = 100)


kernel_estimate <- function(x, type = "Gaussian", scaling = TRUE, ...) {
  if (type %in% c("Gaussian", "normal")) {
    K <- function(X) {
      (1 / (sqrt(2 * pi))) * (exp(1)^((-1 / 2) * X^2))
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
  } else if (type %in% c("binorm", "bivariate", "bivariate normal")) {
    K <- function(X) {
      if (scaling) {
        X <- scale(X)
      }
      x <- unname(unlist(X[, 1]))
      y <- unname(unlist(X[, 2]))
      1 / (2 * pi) * exp((-1 / 2) * (x^2 + y^2))
    }
  } else if (type == "Epanechikov") {
    K <- function(X) {
      if (scaling) {
        X <- scale(X)
      }
      x <- unname(unlist(X[, 1]))
      y <- unname(unlist(X[, 2]))
      unlist(lapply(1:length(x), function(i) {
        if ((x[i]^2 + y[i]^2) < 1) {
          2 / pi * (1 - x[i]^2 - y[i]^2)
        } else {
          0
        }
      }))
    }
  }
  K(x)
}

data(mtcars)
X <- mtcars[c("mpg", "disp")]
dtest <- data.frame()
is.null(ncol(X))

density_estimate <- function(X, K = "normal", h = length(X)^(-1 / 5),scaling=TRUE ...) {
    density_est <- rep(0, N)
  if (is.null(ncol(X))) {
    N <- length(X)
    for (x in X) {
      density_est <- density_est + kernel_estimate(unlist(lapply(X, function(xi) {
        (x - xi) / h
      })), type = K) / (N * h)
    }
  } else {
    if (!K %in% c("binorm", "bivariate", "bivariate normal", "Epanechikov")) {
      stop("Density method not mathing bivatiate setting!")
    } else {
        if (scaling){
        for (i in nrow(X)) {
            density_est <- density_estimate + kernel_estimate(unlist(lapply(1:nrow(X), function(x){
                                                                                

      })), scaling=scaling, type=K) / (n * h * h)
        }
    }
  }
  }

  data.frame(x = sort(X), d = density_est)
}
data(galaxy)
density_estimate(galaxy)

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
    # geom_segment(data = tibble(x = sort(X)), aes(x = x, xend = x, y = 0, yend = h / 100)) +
    geom_path(data = density_est, aes(x, d)) +
    # geom_point(data = density_est, aes(x, d)) +
    theme_light()
}
density_plot(galaxy, h = 1.002, K = "normal")


bivariate_density_plot <- function(dataset, title = "", xlab = "", ylabl = "", ...) {
  names(dataset) <- c("x", "y")
  ggplot(dataset, aes(x, y)) +
    geom_jitter(color = "#e34234", alpha = 1 / 3) +
    geom_point(color = "#e34234") +
    geom_density2d(linetype = "dashed", size = 0.5, color = "#49579F") +
    labs(
      title = title,
      xlab = xlab,
      ylab = ylab
    ) +
    theme_light()
}


bivariate_density_plot(mtcars[, 3:4])


library(car)
scatterplotMatrix(mtcars)


scatterplotmatrix <- function(data) {

}

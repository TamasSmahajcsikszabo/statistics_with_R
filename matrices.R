library(tidyverse)

# matrix multiplication

A <- matrix(c(1, 2, 3, 4, 5, 6), byrow=TRUE, nrow = 2) 
B <- matrix(c(1, 2, 3, 4, 5, 6), byrow=FALSE, ncol = 2) 
C <- A %*% B
# check of calculation
C[1,1] == A[1,1] * B[1,1] + A[1,2] * B[2,1] + A[1,3] * B[3,1]


# 2 X 3 * 3 X 7

A  <-  matrix(rnorm(6), byrow = TRUE, nrow = 2)
V  <-  matrix(rnorm(21), byrow = FALSE, ncol = 7)
R  <- A %*% V
print(paste0('number of rows: ', nrow(R) ,'; number of columns: ', ncol(R)))


# 2 X 2 * 2 X 1

A  <-  matrix(rnorm(4), byrow = TRUE, nrow = 2)
V  <-  matrix(rnorm(2), byrow = FALSE, ncol = 1)
R  <- A %*% V
print(paste0('number of rows: ', nrow(R) ,'; number of columns: ', ncol(R)))



# 1 X 9 * 9 X 1

A  <-  matrix(seq(1,9,1), byrow = TRUE, nrow = 1)
V  <-  matrix(seq(1, 9, 1), byrow = FALSE, ncol = 1)
R  <- A %*% V
print(paste0('number of rows: ', nrow(R) ,'; number of columns: ', ncol(R)))




# 2 X 2 * 2 X 2

A  <-  matrix(c(3, 4, -1, 8), byrow = TRUE, nrow = 2)
V  <-  matrix(c(2, 3, -2, 7), byrow = FALSE, ncol = 2)
R  <- A %*% V


# 3 X 3 * 3 X 2

A  <-  matrix(c(4, 3, 2, 1, -2, -4, -5, 8, 11), byrow = TRUE, nrow = 3)
V  <-  matrix(c(3, -2, 1, -7, 6, -2), byrow = FALSE, ncol = 2)
R  <- A %*% V
print(paste0('number of rows: ', nrow(R) ,'; number of columns: ', ncol(R)))

# example for linear transformation
v <- c(3,2)
M <- matrix(c(1, 2, 2, -2), byrow = TRUE, ncol = 2)

as_vector_plot <- function(v){
  data <- data.frame(x = 0, y = 0)
  new_point <- data.frame(x= v[1], y = v[2])
  
  bind_rows(data, new_point)
}

as_product_plot <- function(v, M){
  product <- v %*% M
  product_vec <- as.vector(product)
  as_vector_plot(product_vec)

}

ggplot() +
  geom_point(data = as_vector_plot(v), aes(x,y))+
  geom_segment(data = as_vector_plot(v), aes(x = 0, y = 0, xend = max(x), yend = max(y)), arrow = arrow(length = unit(0.02, "npc"))) +
  geom_text(data = as_vector_plot(v), aes(x = max(x), y = max(y), label = paste0('Vector p of (', max(x), "; ", max(y), ")")), vjust = -2) +
  geom_point(data = as_product_plot(v, M), aes(x,y))+
  geom_segment(data = as_product_plot(v, M), aes(x = 0, y = 0, xend = max(x), yend = max(y)), arrow = arrow(length = unit(0.02, "npc"))) +
  geom_text(data = as_product_plot(v, M), aes(x = max(x), y = max(y), label = paste0('Vector p of (', max(x), "; ", max(y), ")")), vjust = -2) +
  scale_x_continuous(breaks = seq(1, max(as_product_plot(v, M)[1]) * 5, 1), limits = c(0, max(as_product_plot(v, M)[1]) * 2)) +
  ylim(0, v[2] * 5)

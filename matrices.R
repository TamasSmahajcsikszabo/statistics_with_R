library(tidyverse)
library(ggrepel)

# matrix multiplication

A <- matrix(c(1, 2, 3, 4, 5, 6), byrow = TRUE, nrow = 2)
B <- matrix(c(1, 2, 3, 4, 5, 6), byrow = FALSE, ncol = 2)
C <- A %*% B
# check of calculation
C[1, 1] == A[1, 1] * B[1, 1] + A[1, 2] * B[2, 1] + A[1, 3] * B[3, 1]


# 2 X 3 * 3 X 7

A <- matrix(rnorm(6), byrow = TRUE, nrow = 2)
V <- matrix(rnorm(21), byrow = FALSE, ncol = 7)
R <- A %*% V
print(paste0("number of rows: ", nrow(R), "; number of columns: ", ncol(R)))


# 2 X 2 * 2 X 1

A <- matrix(rnorm(4), byrow = TRUE, nrow = 2)
V <- matrix(rnorm(2), byrow = FALSE, ncol = 1)
R <- A %*% V
print(paste0("number of rows: ", nrow(R), "; number of columns: ", ncol(R)))



# 1 X 9 * 9 X 1

A <- matrix(seq(1, 9, 1), byrow = TRUE, nrow = 1)
V <- matrix(seq(1, 9, 1), byrow = FALSE, ncol = 1)
R <- A %*% V
print(paste0("number of rows: ", nrow(R), "; number of columns: ", ncol(R)))




# 2 X 2 * 2 X 2

A <- matrix(c(3, 4, -1, 8), byrow = TRUE, nrow = 2)
V <- matrix(c(2, 3, -2, 7), byrow = FALSE, ncol = 2)
R <- A %*% V


# 3 X 3 * 3 X 2

A <- matrix(c(4, 3, 2, 1, -2, -4, -5, 8, 11), byrow = TRUE, nrow = 3)
V <- matrix(c(3, -2, 1, -7, 6, -2), byrow = FALSE, ncol = 2)
R <- A %*% V
print(paste0("number of rows: ", nrow(R), "; number of columns: ", ncol(R)))

# custom function for matrix multiplication
`%**%` <- function(A, B){
    # expects two matrices as inputs
    if (ncol(A) != nrow(B)) {
        print("Check matrix sizes, they don't match!")
    }
    result  <- matrix(nrow = nrow(A), ncol = ncol(B))
    for (r in seq(1, nrow(A))) {
        for (c in seq(1, ncol(B))) {
            row_elements  <- A[r,]
            column_elements <- B[,c]
            prod <- 0
            for (e in seq(1, length(row_elements))){
                prod <- prod + row_elements[e] * column_elements[e]
            }
            result[r,c] <- prod
        }
    }
    result
}

A %**% B

# example for linear transformation
v <- c(3, 2)
M <- matrix(c(1, 2, 2, -2), byrow = TRUE, ncol = 2)

v %*% M

as_vector_plot <- function(v) {
    # produces DF of coordinates to plot v() vector from origo
  data <- data.frame(x = 0, y = 0)
  new_point <- data.frame(x = v[1], y = v[2])
  bind_rows(data, new_point)
}

as_product_plot <- function(v, M) {
    # produces DF of coordinates of product of v vector and M matrix
  product <- v %*% M
  product_vec <- as.vector(product)
  as_vector_plot(product_vec)
}

show_vector <- function(v) {
    paste0("[ ", paste0(round(v,2), collapse=", "), " ]")
}

show_matrix <- function(M) {
    matrix_str <- ""
    for (m in nrow(M)-1) {
        row_str <- paste0(show_vector(M[m,]), "\n")
        matrix_str  <- paste0(matrix_str, row_str)
    }
    matrix_str  <- paste0(matrix_str, show_vector(M[nrow(M),]))
    matrix_str

}

transform_vector <- function(v, M) {
    # takes v vector and M matrix
    ggplot() +
      geom_point(data = as_vector_plot(v), aes(x, y)) +
      geom_segment(data = as_vector_plot(v), aes(x = 0, y = 0, xend = max(x), yend = max(y)), arrow = arrow(length = unit(0.02, "npc"))) +
      geom_text(data = as_vector_plot(v), aes(x = max(x), y = max(y), label = paste0("Vector p of (", max(x), "; ", max(y), ")")), vjust = -2) +
      geom_point(data = as_product_plot(v, M), aes(x, y)) +
      geom_segment(data = as_product_plot(v, M), aes(x = 0, y = 0, xend = max(x), yend = max(y)), arrow = arrow(length = unit(0.02, "npc"))) +
      geom_text(data = as_product_plot(v, M), aes(x = max(x), y = max(y), label = paste0("Vector p` of (", max(x), "; ", max(y), ")")), vjust = -2) +
      scale_x_continuous(breaks = seq(1, max(as_product_plot(v, M)[1]) * 5, 1), limits = c(0, max(as_product_plot(v, M)[1]) * 2)) +
      ylim(0, v[2] * 5) +
      labs(title = paste0("Plotting product of v vector and M matrix"),
      subtitle = paste0(show_matrix(M), " multiplies ", show_vector(v)))
}

transform_vector(v, M)


# matrix transformations

x <- 4
y <- 2
v <- c(3, 5)
k  <- 3
theta <- -30

shearing <- function(v, k) {
    base_vectors  <- derive_basis_vectors(v)
    x_shearing <- matrix(c(1, 0, k, 1), byrow = FALSE, ncol = 2)
    y_shearing <- matrix(c(1, k, 0, 1), byrow = FALSE, ncol = 2)
    x_sheared <- x_shearing %*% v
    y_sheared <- y_shearing %*% v
    x <- as_vector_plot(x_sheared)
    y <- as_vector_plot(y_sheared)
    ggplot() +
        geom_point(data = as_vector_plot(v)[2,], aes(x,y)) +
        geom_point(data=x[2,], aes(x,y), color = "cornflowerblue") +
        geom_point(data=y[2,], aes(x,y), color = "cornflowerblue") +
        xlim(0, 20) +
        ylim(0, 20) +
        geom_segment(aes(x=0, y=0, xend=as_vector_plot(v)[2,1], yend=as_vector_plot(v)[2,2]), arrow=arrow()) +
        geom_segment(aes(x=0, y=0, xend=x[2,1], yend=x[2,2]), linetype="dashed", arrow=arrow(), color = "cornflowerblue") +
        geom_text(aes(x=x[2,1], y=x[2,2]),label="shearing along X axis,\n Y coordinate is unchanged", color = "cornflowerblue", vjust=-1) +
        geom_segment(aes(x=0, y=0, xend=y[2,1], yend=y[2,2]), linetype="dashed", arrow=arrow(), color = "cornflowerblue") +
        geom_text(aes(x=y[2,1], y=y[2,2]),label="shearing along Y axis,\n X coordinate is unchanged", color = "cornflowerblue", vjust=-1) +
        labs(
             title = "Shearing along X and Y axis by matrix transformation",
             x = "X",
             y = "Y",
             subtitle = paste0("The base vector is ",show_vector(v)),
             caption = paste0('k = ',k)
        )

}

shearing(c(3,3), 2)

ggplot() +
  geom_point(data = as_vector_plot(shearing(v, 4)), aes(x, y)) +
  geom_point(data = as_vector_plot(v), aes(x, y))


derive_basis_vectors <- function(v) {
    # decomposes vectors to the basis vectors
  vec1 <- c(v[1], 0)
  vec2 <- c(0, v[2])
  matrix(c(vec1, vec2), byrow = TRUE, ncol = 2)
}




rotation <- function(v, theta) {
    # radius to degree
  theta <- theta * pi / 180
  v %*% matrix(c(cos(theta), (-1) * sin(theta), sin(theta), cos(theta)), byrow = TRUE, ncol = 2)
}

draw_coordinate_system <- function() {
    ggplot()+
        geom_segment(aes(x=-Inf, y=0, xend=Inf, yend=0)) +
        geom_segment(aes(x=0, y=-Inf, xend=0, yend=Inf)) +
        labs (x = "X",
              y = "Y")
}

rotate_vector <- function(v, theta, magnify=2) {
    original <- data.frame(t(v))
    rotated <- as.data.frame(rotation(unlist(original), theta))

    draw_coordinate_system() +
        geom_segment(data=original, aes(x=0,y=0, xend=X1, yend=X2), arrow=arrow(length=unit(0.02,"npc"))) +
        geom_segment(data=rotated, aes(x=0,y=0, xend=V1, yend=V2), arrow=arrow(length=unit(0.02,"npc")), color = "darkblue") +
        xlim(-max(original)*magnify, max(original)*magnify) +
        ylim(-max(original)*magnify, max(original)*magnify) +
        geom_text_repel(data=original,aes(X1, X2, label = paste0("Original vector of ", show_vector(v))), vjust=-.075) +
        geom_text_repel(data=rotated,aes(V1, V2, label = paste0("Rotated vector of ", show_vector(unlist(rotated)))), color="darkblue")

}
rotate_vector(c(6,5), -90, magnify=1.4)

# conjugate transpose
Conj(t(rotation(v, theta)))

# cofactor matrix

M <- matrix(c(3, 4, -2, -2, -2, 1, 1, 1, -7), byrow = TRUE, nrow = 3)

get_cofactor <- function(M, i, j) {
  M_cof <- matrix(M[-i, -j], byrow = FALSE, nrow = nrow(M) - 1)

  if (sum(i, j) %% 2 == 0) {
    M_cof
  } else {
    M_cof * (-1)
  }
}

estimate_determinant <- function(M) {
  M[1, 1] * M[2, 2] - M[1, 2] * M[2, 1]
}

get_cofactor_matrix <- function(M, estimae = FALSE, adjoint = TRUE) {
  i <- seq(1, nrow(M))
  j <- seq(1, ncol(M))
  m <- matrix(rep(0, (nrow(M) - 1) * (ncol(M) - 1)), nrow = nrow(M) - 1, ncol = ncol(M) - 1)
  M_cof <- matrix(rep(list(m), length(M)), nrow = nrow(M))

  for (i_i in i) {
    for (j_i in j) {
      M_cof[i_i, j_i][[1]] <- get_cofactor(M, i_i, j_i)
    }
  }
  if (!estimate) {
    M_cof
  } else {
    M_est <- matrix(nrow = nrow(M), ncol = ncol(M))
    for (i_i in i) {
      for (j_i in j) {
        det <- estimate_determinant(M_cof[i_i, j_i][[1]])

        if (sum(i_i, j_i) %% 2 != 0) {
          det <- (-1) * det
        }

        M_est[i_i, j_i] <- det
      }
    }
    if (adjoint) {
      t(M_est)
    } else {
      M_est
    }
  }
}

get_cofactor_matrix(M, adjoint = TRUE, estimate = TRUE)
get_cofactor_matrix(M, estimate = TRUE)



# get determinant

get_determinant <- function(M, reference_no = 1, reference_dim = "row") {
  if (reference_dim == "row") {
    reference <- M[reference_no, ]
    cofactor_matrix <- get_cofactor_matrix(M)[reference_no, ]
  } else if (reference_dim == "col") {
    reference <- M[, reference_no]
    cofactor_matrix <- get_cofactor_matrix(M)[, reference_no]
  } else {
    stop("Please supply either 'col' or 'row' as the reference dimension!")
  }
  det <- 0
  for (i in seq_along(reference)) {
    if (i %% 2 == 0) {
      reference[i] <- reference[i] * (-1)
    }
    det <- det + reference[i] * estimate_determinant(cofactor_matrix[i][[1]])
  }
  det
}

get_determinant(M)


get_inverse <- function(M) {
  det <- get_determinant(M)
  adjoint <- get_cofactor_matrix(M, estimate = TRUE, adjoint = TRUE)

  1 / det * adjoint
}

get_inverse(M)
# verification: I = M * M^-1
M %*% get_cofactor_matrix(M, estimate = TRUE, adjoint = TRUE) * (1 / get_determinant(M))

# book exercise verification; 5.12
m <- matrix(c(4, -2, -2, 2, 8, 4, 30, 12, -4), byrow = TRUE, nrow = 3)
dx <- matrix(c(10, -2, -2, 32, 8, 4, 24, 12, -4), byrow = TRUE, nrow = 3)
dy <- matrix(c(4, 10, -2, 2, 32, 4, 30, 24, -4), byrow = TRUE, nrow = 3)
dz <- matrix(c(4, -2, 10, 2, 8, 32, 30, 12, 24), byrow = TRUE, nrow = 3)


d <- get_determinant(m)
dx <- get_determinant(dx)
dy <- get_determinant(dy)
dz <- get_determinant(dz)

x <- dx / d
y <- dy / d
z <- dz / d


# 5.13.
m <- matrix(c(2, 1, 1, 1, -2, 3, -3, 2, -2), byrow = TRUE, nrow = 3)
get_determinant(m)
get_cofactor_matrix(m, estimate = TRUE, adjoint = TRUE)t


# regression
library(tibble)
data <- tribble(
  ~x, ~y,
  0, 56.751,
  0.2987, 57.037,
  0.4648, 56.979,
  0.5762, 57.074,
  0.8386, 57.42
) 
reg <- lm(data=data, y ~ x)
predictions <- data.frame(y_hat = predict(reg, data))

data <- cbind(data, predictions)
coefs <- reg$coef
beta0  <-coefs[1]
beta1 <- coefs[2]


ggplot()+
    geom_point(data=data, aes(x,y)) +
    geom_abline(aes(intercept = beta0, slope = beta1)) +
    geom_segment(data=data, aes(x=x, y=y_hat, xend=x, yend=y), linetype = "dashed", color = "grey50")

A <- seq(1,10)
B <- seq(11, 23)
A <- matrix(seq(1,9), ncol = 3)
B <- matrix(seq(1,9), ncol = 3)

# Kronecker product
`%K%` <- function(A, B){
    if (all(is.vector(A), is.vector(B))){
        print("vectors")
        result <- matrix(nrow = length(A), ncol = length(B))
        for (i in seq(length(A))){
            for (j in seq(length(B))){
                result[i, j]  <- A[i] * B[j]
            }
        }
    } else if (all(is.matrix(A), is.matrix(B))) {
        print("matrices")
        result  <-  matrix(rep(list(B), length(A)), ncol = ncol(A))
        for (i in seq(nrow(A))){
            for (j in seq(ncol(A))){
                est <- B * A[i, j]
                result[i,j][[1]] <- est
            }
        }
    } else if(is.vector(A) || is.vector(B)) {
        print("transform")
        if (is.vector(A)){
            A <- matrix(A, nrow=1)
        } else if(is.vector(B)){
            B <- matrix(B, nrow=1)
        }
        A %K% B
    }

    result
}
v <- sample(1:9, 7)
v %K% v
Kronecker <- M %K% M
Kronecker[2,2]

#Hadamard product
check_size <- function(A, B){
    if (all(ncol(A) == ncol(B), nrow(A) == nrow(B))) {
        test <- TRUE
    } else {
        test  <- FALSE
    }
    test
}

`%H%` <- function(A, B){
    if (all(is.matrix(A), is.matrix(B))){
        if (check_size(A, B)){
            result <- matrix(ncol=ncol(A), nrow=nrow(A))
            for (i in seq(nrow(A))) {
                for (j in seq(ncol(A))){
                    result[i, j] <- A[i, j] * B[i, j] 
                }
            }
        } else{
            result  <- stop("Please check if matrix sizes of the inputs do match!")
        }

    } else{
        result <- stop("You have to provide matrices as inputs!")
    }
    result
}

A = matrix(c(2,3,4,5), nrow=2)
A %H% A


# exercises 5.7

A  <- matrix(c(1, 2, -1, 1), nrow=2, byrow = TRUE)
B  <- matrix(c(2, 3, 4, 1), nrow=2, byrow = TRUE)
A %H% B

C <- matrix(c(2, -0.3, 1, 1.5, 7, -0.4), nrow = 2, byrow=TRUE)
D <- matrix(c(1.4, 9, 0.5, 8, -0.1, 10), nrow = 2, byrow=TRUE)
C %H% D


# exercises 5.8.
B <- c(3, -1, 4)
r <- A %K% B
r[2,2]


A <- c(-2, -3)
B <- matrix(c(0, 1, 2, 3, 4, 5, 6, 7, 8), nrow=3, byrow = TRUE)
B %K% B

generate_identity_matrix <- function(size=3){
    result <- matrix(ncol=size, nrow=size)
    for (i in seq(size)){
        for (j in seq(size)) {
            if (i == j){
                result[i,j] <- 1
            } else {
                result[i, j] <- 0
            }
        }
    }
    result
}
generate_identity_matrix(10)

is_diagonal_matrix <- function(m){
    diagonal <- c()
    not_diagonal <- c()
    for (i in seq(nrow(m))){
        for (j in seq(ncol(m))) {
            if (i==j){
                diagonal <- c(diagonal, (m[i, i] != 0))
            } else if (i!=j) {
                not_diagonal <- c(not_diagonal, (m[i,j] != 0))
            }
        }
    }
    sum(diagonal) == length(diagonal) && sum(not_diagonal) == 0
}

is_triangular_matrix <- function(m, direction = "upper") {
    if (direction == "upper"){
        upper_test <- c()
        lower_test <- c()
        for (i in seq(nrow(m))){
            for (j in seq(nrow(m))){
                if (i > j) {
                    lower_test <- c(lower_test, m[i,j] != 0)
                } else if (i <= j) {
                    upper_test <- c(upper_test, m[i,j] != 0)
                }
            }
        }
        test <- sum(upper_test) == length(upper_test) && sum(lower_test) == 0
    } else if (direction == "lower"){
        upper_test <- c()
        lower_test <- c()
        for (i in seq(nrow(m))){
            for (j in seq(nrow(m))){
                if (i < j) {
                    upper_test <- c(upper_test, m[i,j] != 0)
                } else if (i >= j) {
                    lower_test <- c(lower_test, m[i,j] != 0)
                }
            }
        }
        test <- sum(lower_test) == length(lower_test) && sum(upper_test) == 0
    }
    test
}

is_symmetric_matrix <- function(m, skew=FALSE){
    # skew: Boolean, if TRUE, checks for skew-symmetry
    tests <- c()
    if (!skew){
        for (i in seq(nrow(m))){
            for (j in seq(ncol(m))){
                tests <- c(tests, m[i,j] == m[j,i])
            }
        }
    } else if (skew){
        for (i in seq(nrow(m))){
            for (j in seq(ncol(m))){
                tests <- c(tests, m[i,j] == (-1) * m[j,i])
            }
        }
    }
    sum(tests) == nrow(m) * nrow(m)
}

m  <- matrix(c(1,0,0,3,-2,0,-6,6,-4), nrow = 3, byrow=3)
n <- matrix(c(2,3,4,0,4,5,0,0,7), nrow=3, byrow = 3)
is_triangular_matrix(m, "lower")
is_triangular_matrix(m, "upper")
is_triangular_matrix(n, "upper")
is_triangular_matrix(n, "lower")
is_symmetric_matrix(generate_identity_matrix(100))
is_symmetric_matrix(m)
is_symmetric_matrix(n)

q  <- matrix(c(0,-12,-21, 12,0,-9,21,9,0), nrow=3, byrow=TRUE)
is_symmetric_matrix(q, skew=T)

is_logical_matrix <- function(m){
    tests <- c()
    logical_test <- function(x){
        x %in% c(0,1) || x %in% c(TRUE, FALSE) 
    }
    for (i in seq(nrow(m))){
        for (j in seq(ncol(m))){
            tests <- c(tests, logical_test(m[i, j]))
        }
    }

    sum(tests) == length(m)
}


vec <- sample(c(TRUE, FALSE), 27, replace=TRUE)
vecm <- matrix(vec, nrow=3)
is_logical_matrix(vecm)

vecm <- matrix(vec, nrow=3)
vecm[2,2] <- "bug"
is_logical_matrix(vecm)

library(ggplot2)
library(tibble)


# functions

estimate_k <- function(W, L, C, V) {
  # k is the optimal distance between vertices; it's the radius of the empty area around a vertex
  # constant C is found experimentally

  area <- W * L
  n_vertices  <- length(V)
  C * sqrt(area / n_vertices)
}

# force declarations; where d is the distance between two vertices
#attractve
force_attract <- function(d, k, params = "d^2 / k"){
  eval(parse(text = params))
}

#repulsive
force_repulsive <- function(d, k, params = "(-1) * (k ^2 / d)") {
  eval(parse(text = params))
}


d <- seq(1, 100)

res <- data.frame(matrix(ncol=3)) 
colnames(res) <- c("distance", "attractive", "repulsive") 
k  <- estimate_k(100, 100, 1.5, d) 

for (i in d) {
    res[i, "distance"] <- i 
    res[i, "attractive"] <- force_attract(i, k)
    res[i, "repulsive"] <- force_repulsive(i, k)
}
res$sum = res$attractive + res$repulsive
zero_d = res[res$sum ==0, ]$d
ggplot(res, aes(x = distance), alpha = 0/5, size = 4)+
    geom_line(aes(y = attractive), color = "cornflowerblue") + 
    geom_line(aes(y = repulsive), color = "coral") +
    geom_line(aes(y = attractive + repulsive), color = "gray39") +
    geom_point(aes(x = zero_d, y = -1)) +
    geom_text(aes(x = zero_d, y = -1, label = "k, the ideal distance of vertices \n where forces cancel each other"), hjust = -0.25, vjust= 0.75) +
    ylim(-251,250)

test_input <- tibble(
         "vertex_a" = c("a", "b", "c"),
         "vertex_b" = c("b", "c", "a"),
         "strength" = c(5, 4, 3)
  )

network_graph <- function(input) {
  W  <- seq(1, ncol(input) * 100)
  L  <- seq(1, ncol(input) * 100)
  area  <-W * L
}

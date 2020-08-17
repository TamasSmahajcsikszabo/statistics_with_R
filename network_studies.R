library(ggplot2)


# functions

estimate_k <- function(x, y, C, V){
  C*sqrt((x * y) / (length(V)))
}

force_attract <- function(d, k){
  d^2 / k
}

force_repulsive <- function(d, k) {
  (-1) * k^2 /d
}


d <- seq(1, 1000)

res <- data.frame(matrix(ncol=3)) 
colnames(res) <- c("distance", "attractive", "repulsive") 
k  <- estimate_k(100, 100, 1.5, 10) 

for (i in d) {
    res[i, "distance"] <- i 
    res[i, "attractive"] <- force_attract(i, k)
    res[i, "repulsive"] <- force_repulsive(i, k)
}

ggplot(res, aes(x = distance), alpha = 1/5, size = 4)+
    geom_line(aes(y = attractive), color = "cornflowerblue") + 
    geom_line(aes(y = repulsive), color = "coral") +
    geom_line(aes(y = attractive + repulsive), color = "gray40") +
    ylim(-10000,10000)

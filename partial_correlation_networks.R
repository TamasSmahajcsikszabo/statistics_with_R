# matrix inverse
m <- matrix(sample(rnorm(100)), ncol=10, nrow=10)
i <- solve(m)
i * m

x <- rnorm(100)
y  <- x * rnorm(100) + x
z  <- rnorm(100) * (1/x)
m <- matrix(c(x, y, z), ncol=3)

cov_m <- cov(m)
solve(cov_m)


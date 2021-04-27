function (data, k, cut = "number", method, scaling = TRUE, noisecut = 0, 
          ...) 
{
  if (!identical(scaling, FALSE)) 
    sdata <- scale(data, center = TRUE, scale = scaling)
  else sdata <- data
  n <- nrow(data)
  noise <- FALSE
  c1 <- hclust(dist(sdata), method = method)
  if (cut == "number") 
    partition <- cutree(c1, k = k)
  else partition <- cutree(c1, h = k)
  cl <- list()
  nc <- max(partition)
  clsizes <- numeric(0)
  for (i in 1:nc) clsizes[i] <- sum(partition == i)
  ncn <- sum(clsizes > noisecut)
  if (ncn < nc) {
    noise <- TRUE
    newcln <- (1:nc)[clsizes > noisecut]
    nc <- ncn + 1
    newpart <- rep(nc, n)
    for (i in 1:ncn) newpart[partition == newcln[i]] <- i
    partition <- newpart
  }
  for (i in 1:nc) cl[[i]] <- partition == i
  out <- list(result = c1, noise = noise, nc = nc, clusterlist = cl, 
              partition = partition, clustermethod = "hclust/cutree")
  out
}

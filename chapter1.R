library(WRS)

m <- matrix(c(1,4,5,7,8,1), ncol = 3)
mean(m)

#row wise operation
apply(m, 1, mean)

# column wise operation
apply(m, 2, mean)


# splitting a dataframe by factor values

bp <- sample(seq(110,170), 100, replace = TRUE)
group <- sample(c("E", "P"), 100, replace = TRUE)

bp_data <- data.frame(
                      "blood_pres" = bp,
                      "group" = group
)

split(bp, group)
fac2list(bp, group)

#storing differing length variables as lists
DEP = list()
DEP[[1]] = sample(seq(2,8), 124, replace = TRUE)
DEP[[2]] = sample(seq(9,12), 35, replace = TRUE)
DEP[[3]] = sample(seq(13,20), 12, replace = TRUE)

lapply(DEP, var)
lapply(DEP, mean)
lapply(DEP, sd)
barplot(unlist(lapply(DEP, mean)))

#filtering
quake <- read.table("quake.txt", header =TRUE)
mean(quake[,2])
flag <- quake[,2] < 360
flag <- which(quake[,2] < 360) # return row numbers
barplot(c(mean(quake[flag,2]), mean(quake[,2])))

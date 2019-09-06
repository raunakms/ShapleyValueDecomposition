set.seed(12345)

#### RANDOM NUMBER GENERATOR ----
grp <- sample(x=LETTERS[1:8], size=10000, replace=TRUE)
obs <- sample(x=1:999999, size=10000, replace=TRUE)

### PREPARE DATA ---
dat <- data.frame(Group=grp, Observation=obs)
dat$Group <- as.character(dat$Group)


### RUN SHAPLEY VALUE DECOMPOSITON: PARALLELIZED VERSION ---
source("ShapleyValueDecomposition_parallel.R")
ShapleyValue.Decomposition.parallel(dat=dat, n_cores=4)

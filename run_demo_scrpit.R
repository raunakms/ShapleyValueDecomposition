### DEFINE PATH ---
dir.wrk <- getwd()

### DEFINE FILE ---
file.dat <- file.path(dir.wrk, "dummy_observations.tsv")

### LOAD DATA ---
dat <- read.delim(file.dat, header=TRUE, stringsAsFactors=FALSE)

### RUN SHAPLEY VALUE DECOMPOSITON ---
source("ShapleyValueDecomposition.R")
ShapleyValue.Decomposition(dat=dat)


### RUN SHAPLEY VALUE DECOMPOSITON: PARALLELIZED VERSION ---
source("ShapleyValueDecomposition_parallel.R")
ShapleyValue.Decomposition.parallel(dat=dat, n_cores=4)

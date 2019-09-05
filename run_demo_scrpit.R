### DEFINE PATH ---
dir.wrk <- getwd()

### DEFINE FILE ---
file.dat <- file.path(dir.wrk, "dummy_observations.tsv")
file.function <- file.path(dir.wrk, "ShapleyValueDecomposition.R")

### LOAD DATA ---
dat <- read.delim(file.dat, header=TRUE, stringsAsFactors=FALSE)

### RUN SHAPLEY VALUE DECOMPOSITON ---
source(file.function)
ShapleyValue.Decomposition(dat=dat)

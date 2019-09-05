### DEFINE PATH ---
dir.wrk <- getwd()

### DEFINE FILE ---
file.dat <- file.path(dir.wrk, "dummy_observations.tsv")

### LOAD DATA ---
dat <- read.delim(file.dat, header=TRUE, stringsAsFactors=FALSE)

### RUN SHAPLEY VALUE DECOMPOSITON ---
list.df <- ShapleyValue.Decomposition(dat)

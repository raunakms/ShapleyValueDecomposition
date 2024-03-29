# ShapleyValueDecomposition
This is script useful for Shapley Value Decomposition implemented in R. This script is based on the Shapley Value Decomposition formulation presented by [Anthony F. Shorrocks, 2013](https://link.springer.com/article/10.1007%2Fs10888-011-9214-z).

## Reference
> Anthony F. Shorrock. *Decomposition procedures for distributional analysis: a unified framework based on the Shapley value.* J Econ Inequal (2013) 11:99–126. [DOI: 10.1007/s10888-011-9214-z](https://link.springer.com/article/10.1007%2Fs10888-011-9214-z)
<br/><br/>

## Usage
- Shapley Value Decomposition runs in two modes 
  - **Un-parallelized mode** uses `ShapleyValue.Decomposition()` function. This is suitable for a small dataset and 
  - **Parallelized mode** uses `ShapleyValue.Decomposition.parallel()` function. This is most suitable for a large dataset. It dependes on `foreach` and `doParallel` R-packages.
- See `run_demo_script.R` for details.

```sh
# UN-PARALLELIZED MODE ---
source("ShapleyValueDecomposition.R")
ShapleyValue.Decomposition(dat)

# PARALLELIZED MODE ---
source("ShapleyValueDecomposition_parallel.R")
ShapleyValue.Decomposition.parallel(dat, n_cores=4)
```
#### Input parameter
- `dat` : R dataframe containing the input data of Group and Observation. See File Format for details.
- `n_cores` : The number of cores to use for parallel execution. 

#### Output
- The function `ShapleyValue.Decomposition` generates output of the following in form of a R-list.
  - `G`   : Gini Index (global)
  - `G_k` : Gini Index per group
  - `W`   : Within group inequality decomposition
  - `W_k` : Within group inequality per group
  - `B`   : Between group inequality decomposition
  - `O`   : Overlap Effect
<br/><br/>

## File Format
- A tab separated file with two columns.
- The first column with header `Group`. This column includes group labels.
- The second column with header `Observation`. This column contains numeric values indicating individual observation or income amount. These values may or may not be in a sorted order.
- See `dummy_observations.tsv` for details. 

```sh
Group   Observation
A       2
A       6
B       10
C       18
B       20
A       25
C       30
C       50
C       55
B       84
```
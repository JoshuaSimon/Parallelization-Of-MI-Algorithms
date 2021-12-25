# Parallelization-Of-MI-Algorithms
This repository contains research work on parallelizing MI (multiple imputation) algorithms using R.

Run-time is an issue for any MI algorithm, because there are several time
consuming iterative steps involved, such as the number of iterations in
sequential regression, the identifcation of nearest neighbors, the number of
variables and variable selection schemes or iterative estimation procedures
(Fisher Scoring). Parallelization is a promising way to decrease computation time. Here,
the requirements and implementation for different operating 
systems and compare currently available solutions in terms of usability
and run-time are described.
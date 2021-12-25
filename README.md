# Parallelization-Of-MI-Algorithms
This repository contains research work on parallelizing MI (multiple imputation) algorithms using R.

Run-time is an issue for any MI algorithm, because there are several time
consuming iterative steps involved, such as the number of iterations in
sequential regression, the identifcation of nearest neighbors, the number of
variables and variable selection schemes or iterative estimation procedures
(Fisher Scoring). Parallelization is a promising way to decrease computation time. Here,
the requirements and implementation for different operating 
systems are compared and currently available solutions in terms of usability
and run-time are described.

The `/src` directory contains the R code of the simulation studies conducted for this project. The `/poster` directory contains the LaTeX code and a PDF file for the scientific poster, which was presented as part of the _Statistical Analysis of Incomplete Data_ lecture by Dr. Florian Meinfelder and Paul Messer in the winter terms of 2021/2022 at [Otto-Friedrich-University Bamberg](https://www.uni-bamberg.de/).
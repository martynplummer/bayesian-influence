# UNOS data posterior predictive checks

Using the posterior predictive check framework to assess the variability of the pV/pW.

## Inputs

* `unos-reference.bug` Model for the UNOS data with weakly informative reference prior. This file is identical to `../binomial/unos-reference.bug` but includes 
   replicate values `Y.rep` that will be simulated from the posterior predictive distribution.
* `unos-posterior-check.R`. Fits the refrence model to the UNOS data, simulates replicate data sets, then refits the model to the replicates. Each iteration produces
   a new estimate of the ratio pV/pW. The simulations run very slowly so a progress bar is included if the code is run interactively.

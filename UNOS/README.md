Data originally from the United Network for Organ Sharing (UNOS) and published by Gelfand (2003).
The data do not appear to be directly accessible from the UNOS web site: https://unos.org

The sensitivity analyses are based on perturbations to individual patients. In the binomial
directory, patients in the same centre and age group are aggregated into a binomial outcome.
To get the correct estimate of pV we need to do some additional post-processing. In the
Bernoulli subdirectory, the data are disaggregated into individual patient outcomes. This
model is much slower but shows in principle how pV is estimated from the posterior variance
of the log-likelihood.

Subdirectory posterior-checks carries out posterior predictive simulations to get a 
predictive distribution for pV/pW under the reference model. As shown in the paper, this
test statistic shows high variability.

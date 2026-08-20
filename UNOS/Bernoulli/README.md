# UNOS Bernoulli model

Modelling the UNOS data using a Bernoulli model. The data is disaggregated into individual operations with a binary outcome. This is not strictly necessary but is included here as an additional check. The Bernoulli an binomial models should return the same results

## Inputs

* `read-unos-data.R` Reads the UNOS data and then disaggregates each binomial outcome into individual Bernoulli outcomes.
* `unos-reference.bug` BUGS model using a weakly informative prior.
* `unos-informative.bug` BUGS model using an informative prior. Hyper-parameters a and b which determine the location and concentration of parameter `mu.alpha` must be supplied with the data.
* `unos-figure.R` Recreate figure 4 from the paper (`unos-figure.pdf`)
* `unos-table.R` Recreate table 2 from the paper. The output is a skeleton of table 2 in LaTeX format that needs editing.

## Outputs

* `unos-figure.pdf` Figure 4

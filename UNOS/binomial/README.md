# UNOS binomial model

Modelling the UNOS data using a binomial model.

## Inputs

* `unos-reference.bug` BUGS model using a weakly informative prior.
* `unos-informative.bug` BUGS model using an informative prior. Hyper-parameters a and b which determine the location and concentration of parameter `mu.alpha` must be supplied with the data.
* `unos-figure.R` Recreate figure 4 from the paper (`unos-figure.pdf`)
* `unos-table.R` Recreate table 2 from the paper. The output is a skeleton of table 2 in LaTeX format that needs editing.

## Outputs

* `unos-figure.pdf` Figure 4

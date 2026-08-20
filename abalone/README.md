# Abalone
Abalone data set from the UCI machine learning repository
<https://doi.org/10.24432/C55C7W>

## Workflow

1. Run the file `abalone-fit.R`. This will fit a log-linear gamma GLM to the abalone data and calculate the conformal local leverage, influence, and outlier statistics. These will be saved in the file abalone.RData
2. Run the file `abalone-plot.R`. This will recreate figure 1 from the paper (`abalone-influence.pdf`) highlighting the anomalous observations, and table 1 (`hightab.tex`) giving details of the observations that are highlighted as anomalous.

## Inputs

* `abalone.bug` BUGS model for fitting a gamma GLM to the data.
* `read-data.R` Utility file for reading in the data.
* `abalone-fit.R` Fit the model and save the results.
* `abalone-plot.R` Recreate table 1 and figure 1 from the paper.

## Outputs

* `abalone.RData` Saved conformal diagnostics: clinf, cllev, clout.
* `abalone-influence.pdf` Reproduction of figure 1 from the paper highlighting anomalous observations.
* `hightab.tex` LaTeX snippet showing data for the 3 anomalous observations (table 1).

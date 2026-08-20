# Bikesharing

Bike Sharing data from the UCI machine learning repository
<https://doi.org/10.24432/C5W894>

## Workflow

1. Run the model in `bikesharing-data.R`. Results will be saved in the file
`bikesharing.RData`
2. Run `bikesharing-figures.R` to recreate figures 2 (`bikeshare-clout-daily.pdf`) and 3 (`bikeshare-clout-hourly.pdf`) from the paper.

## Inputs

* `bikesharing.bug` BUGS language model that fits a Poisson regression model to the data.
* `read-data.R` Utility file for reading data from the sub-directory `data`.
* `bikesharing-fit.R` Fit the model and save the results in `bikesharing.RData`.
* `bikesharing-figures.R` Recreate figures 2 and 3 from the article

## Outputs

* `bikesharing.RData` Saved results from fitting the model.
* `bikeshare-clout-daily.pdf` Figure 2 from the paper.
* `bikeshare-clout-hourly.pdf` Figure 3 from the paper.


## Notes

The model in `bikesharing.bug` includes a quadratic effect for feeling
temperature. This models the effect of heatwaves, when excessive
temperatures cause a decrease in bicycle hiring.

The file `bikesharing-plot.R` also creates some plots not used in the
paper.



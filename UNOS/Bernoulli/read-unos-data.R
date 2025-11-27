library(dplyr)

unos <- read.csv("../unos-data.csv")
unos$age <- unos$age/10

## Expand aggregated binomial data into individual Bernoulli outcomes
cases <- reframe(unos, centre=rep(centre, y), age=rep(age, y), y=1)
controls <- reframe(unos, centre=rep(centre, n-y), age=rep(age, n-y), y=0)
unos_binary <- bind_rows(cases, controls)

## Sanity check. Can we reconstruct the binomial data from the Bernoulli data?
unos2 <- unos_binary |>
    group_by(centre, age) |>
    summarize(n=n(), y=sum(y), .groups="drop")

stopifnot(all.equal(unos, unos2, check.attributes=FALSE))

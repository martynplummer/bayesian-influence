abalone <- read.table("data/abalone.data", header=FALSE, sep=",")
names(abalone) <- c("sex","length","diameter","height",
                    "whole.weight", "shucked.weight",
                    "viscera.weight", "shell.weight", "rings")
abalone$id <- 1:nrow(abalone)

## Numeric variables have been scaled to be in range 0-1 by dividing
## by 200 (See data/abalone.names). Reverse the scaling to get heights
## in mm and weights in g.

abalone$length <- abalone$length * 200
abalone$diameter <- abalone$diameter * 200
abalone$height <- abalone$height * 200

abalone$whole.weight <- abalone$whole.weight * 200
abalone$shucked.weight <- abalone$shucked.weight * 200
abalone$viscera.weight <- abalone$viscera.weight * 200
abalone$shell.weight <- abalone$shell.weight * 200

## Remove immature (sexually undifferentiated) abalone
abalone <- subset(abalone, sex != "I")

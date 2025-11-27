### Initial value function for all models
### This is a warm start based on pilot runs for the reference model

initfun <- function() {
    ## Warm start based on pilot runs
    mu.alpha <- rnorm(1, -1.2, 0.084)
    mu.beta <- rnorm(1, 0.079, 0.068)
    tau.alpha <- rlnorm(1, meanlog=3.4, sdlog=1.89)
    tau.beta <- rlnorm(1, meanlog=2.5, sdlog=1.30)
    list("mu.alpha" = mu.alpha,
         "mu.beta" = mu.beta,
         "tau.alpha" = tau.alpha,
         "tau.beta" = tau.beta)
}


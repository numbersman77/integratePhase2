power2logrank <- function(d, alpha, hr, kappa = 1 / 2){

## compute power in 2-sample logrank test
fac <- kappa * (1 - kappa)
res <- pnorm(abs(log(hr)) * sqrt(d * fac) - qnorm(1 - alpha / 2))
return(res)
}

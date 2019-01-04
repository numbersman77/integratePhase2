samplesize2logrank <- function(alpha, beta, hr, kappa = 1 / 2){

## compute samplesize in 2-sample logrank test
fac <- qnorm(1 - alpha / 2) + qnorm(1 - beta)
res <- ceiling(1 / (kappa * (1 - kappa)) * fac ^ 2 / (log(hr) ^ 2))
return(res)
}

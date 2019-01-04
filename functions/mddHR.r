mddHR <- function(d, alpha, kappa = 1 / 2){
  
  ## Minimal detectable difference for HR.
  ## For a two-sided test, this is simply the critical value
  ## of the test of H0: theta = log(HR) = 0.
  ## So, we compute theta from
  ##
  ## theta <= -qnorm(1 - alpha / 2) * SE(theta).
  ##
  ## Similarly, one can think of having the upper confidence interval limit
  ## smaller than 0:
  ##
  ## theta + qnorm(1 - alpha / 2) * SE(theta) <= 0.
  ##
  ## In both cases, this yields
  ##
  ## theta <= - qnorm(1 - alpha / 2) * SE(theta) or, using SE(theta) = sqrt(4 / n)
  ## (only applies if we have 1:1 randomization!)
  ##
  ## HR <= exp(- qnorm(1 - alpha / 2) * sqrt(4 / n)).

  fac <- kappa * (1 - kappa)
  SEs <- (d * fac) ^ (- 1 / 2)
  za <- qnorm(1 - alpha / 2)
  res <- exp(- za * SEs)
  return(res)
}
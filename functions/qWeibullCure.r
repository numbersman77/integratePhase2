qWeibullCure <- function(p, p0, shape, scale){
  res <- rep(NA, length(p))
  ind1 <- (p <= p0)
  ind2 <- (p > p0)
  res[ind1] <- Inf
  res[ind2] <- qweibull(1 - (p[ind2] - p0) / (1 - p0), shape = shape, scale = scale)
  return(res)  
}
EFSfromOS <- function(Corr.EFSOS, n, os, lamOS, medEFS){
  
  medOS <- log(2) / lamOS
  tau <- (Corr.EFSOS * lamOS) / (1 - Corr.EFSOS)
  k <- rpois(n, tau * os)
  efs <- rgamma(n, shape = 1 + k, rate = lamOS + tau)
  efs <- efs * medEFS / medOS
  return(efs)
}



medEFS <- 5
medOS <- 10
lamOS <- log(2) / medOS
os <- rexp(n, rate = lamOS)
Corr.EFSOS <- 0.8
n <- 1000

efs <- EFSfromOS(Corr.EFSOS, n, os, lamOS, medEFS)
efs <- pmin(efs, os)

cor(os, efs)
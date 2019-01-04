landmarkTest <- function(time, event, tmt, t0){
  
  indA <- (tmt == "A")
  tmp <- Surv(time[indA], event[indA]) ~ tmt[indA]
  fit1 <- survfit(tmp, conf.type = 'log-log')
  dat.km1 <- cbind(fit1$time, fit1$surv, fit1$std.err)
  dat.km1 <- dat.km1[dat.km1[, 1] < t0, ]
  dat.km1 <- dat.km1[nrow(dat.km1), ]
  St1 <- ifelse(max(time[indA]) >= t0, dat.km1[2], NA)
  sig1 <- dat.km1[3]

  indB <- (tmt == "B")
  tmp <- Surv(time[indB], event[indB]) ~ tmt[indB]
  fit2 <- survfit(tmp, conf.type = 'log-log')
  dat.km2 <- cbind(fit2$time, fit2$surv, fit2$std.err)
  dat.km2 <- dat.km2[dat.km2[, 1] < t0, ]
  dat.km2 <- dat.km2[nrow(dat.km2), ]
  St2 <- ifelse(max(time[indB]) >= t0, dat.km2[2], NA)
  sig2 <- dat.km2[3]

  # chi^2 test statistic
  # from Klein et al (2007), Stat. Med.
  X1 <- (St1 - St2) ^ 2 / (St1 ^ 2 * sig1 ^ 2 + St2 ^ 2 * sig2 ^ 2)
  X3 <- (log(-log(St1)) - log(-log(St2))) ^ 2 / (sig1 ^ 2 / (log(St1) ^ 2) + sig2 ^ 2 / (log(St2) ^ 2))

  # 0 = do not reject, 1 = reject    
  res <- 1 - pchisq(X3, df = 1)
  return(res)
}
  
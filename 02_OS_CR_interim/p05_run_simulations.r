#--------------------------------------------------------------------------------------------------------------------------------------
# run simulations 
#--------------------------------------------------------------------------------------------------------------------------------------

set.seed(1111)

for (u in run.scenarios){  
  
  scen.u <- scenarios[u, ]
  recruit.u <- scen.u["recruit.prop"] * recruit
  Npt <- sum(recruit.u)
  
  # floor or ceiling, randomly --> we only want "entire" patients per month
  fc <- rbinom(length(recruit.u), 1, prob = recruit.u - floor(recruit.u))
  for (z in 1:length(recruit.u)){
    if (fc[z] == 0){recruit.u[z] <- floor(recruit.u[z])} else {recruit.u[z] <- ceiling(recruit.u[z])}
  }
  
  if (sum(recruit.u) < Npt){recruit.u[length(recruit.u)] <- recruit.u[length(recruit.u)] + (Npt - sum(recruit.u))}
  if (sum(recruit.u) > Npt){recruit.u[length(recruit.u)] <- recruit.u[length(recruit.u)] - (Npt - sum(recruit.u))}
  
  # generate OS data
  GenData0 <- generateData(p0 = scen.u["p1.cr"], p1.corr = scen.u["p1.cr"], HR.OS = 1, mut.rate = scen.u["mut.rate"], 
                           prob.longterm = p.long.term, med.nonresp.H0 = med.nonresp.H0, med.resp.shortterm.H0 = med.resp.shortterm.H0, 
                           long.surv = long.surv, n = n)    
  
  p1.corr <- correctProbCR(OR.CR = scen.u["OR.CR"], p0 = scen.u["p1.cr"], mut.rate = scen.u["mut.rate"])
  GenData1 <- generateData(p0 = scen.u["p1.cr"], p1.corr = p1.corr, HR.OS = scen.u["HR.OS"], mut.rate = scen.u["mut.rate"], 
                           prob.longterm = p.long.term, med.nonresp.H0 = med.nonresp.H0, med.resp.shortterm.H0 = med.resp.shortterm.H0, 
                           long.surv = long.surv, n = n)    
  
  stores <- list(GenData0, GenData1)

  # simulate trials
  study.info <- list(Npt = Npt, Nev = scen.u["Nevs"], N.interim = scen.u["N.interims"], 
                           store0 = stores[[scen.u["dat1"] + 1]], store1 = stores[[scen.u["dat2"] + 1]], 
                           recruit = recruit.u, dropout.month = tau1) 
  sim.u <- simulate_OS(study.info = study.info, nsim = scen.u["nsim"], print.freq = 1000, alpha = scen.u["alpha"])
  
  print(paste("scenario ", u, " of ", length(run.scenarios), " done", sep = ""))
  save(sim.u, file = paste(path, "02_OS_CR_interim/results/simul_results_scenario", u, sep = ""))
}


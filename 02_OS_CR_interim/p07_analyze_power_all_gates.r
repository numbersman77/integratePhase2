#--------------------------------------------------------------------------------------------------------------------------------------
# analyze results from simulations
#--------------------------------------------------------------------------------------------------------------------------------------

# gate combinations to be used (NA means gate is not considered):
or.gates  <- c(2,    2)
os.gates  <- c(NA, 1.1)

analysis2_IA <- matrix(NA, nrow = length(run.scenarios) *  length(or.gates), ncol = ncol(scenarios) + 6)
colnames(analysis2_IA) <- c(colnames(scenarios), "CR gate", "OS gate", "power (no Fut)", "power (consider Fut)", "P(correct stop under H0)", "P(wrong stop under H1)")

iter <- 1

for (u in run.scenarios){
  for (g in 1:length(or.gates)){
    
    # set gates. If NA, then take a gate such that this endpoint is not relevant, i.e. this is then the univariate consideration.
    or.gate.g <- max(-Inf, or.gates[g], na.rm = TRUE)
    analysis2_IA[iter, "CR gate"] <- or.gates[g]

    os.gate.g <- min(Inf, os.gates[g], na.rm = TRUE)
    analysis2_IA[iter, "OS gate"] <- os.gates[g]

    # object sim.u
    load(file = paste(path, "02_OS_CR_interim/results/simul_results_scenario", u, sep = ""))
  
    # define simulation to be analyzed
    sim.in <- sim.u

    # H0 or H1?
    scen.u <- scenarios[u, ]
    h0 <- (scen.u["dat1"] == 0) & (scen.u["dat2"] == 0)
    h1 <- (scen.u["dat1"] == 0) & (scen.u["dat2"] == 1)
  
    analysis2_IA[iter, colnames(scenarios)] <- scen.u[colnames(scenarios)]
    
    # power not considering futility
    analysis2_IA[iter, "power (no Fut)"] <- mean(sim.in$outcome1)

    # define stopping rule
    survive.interim <- (sim.in$oddsratio1 >= or.gate.g) & (sim.in$hr1.os.int <= os.gate.g)
    stop.interim <- (survive.interim == FALSE)
    
    # power considering CR
    analysis2_IA[iter, "power (consider Fut)"] <- sum(sim.in$outcome1[survive.interim]) / scen.u["nsim"]

    # P(correctly stopping under H0)
    if (h0){analysis2_IA[iter, "P(correct stop under H0)"] <-  mean(stop.interim)}
  
    # P(wrongly stopping under H1)
    if (h1){analysis2_IA[iter, "P(wrong stop under H1)"] <-  mean(stop.interim)}
  
    iter <- iter + 1
  }
}

out2 <- analysis2_IA[, c("scenario", "mut.rate", "Nevs", "N.interims", "dat1", "dat2", "OR.CR", "CR gate", "OS gate", "power (no Fut)", 
                     "power (consider Fut)", "P(correct stop under H0)", "P(wrong stop under H1)")]
out2


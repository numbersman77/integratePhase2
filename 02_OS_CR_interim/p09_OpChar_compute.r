#--------------------------------------------------------------------------------------------------------------------------------------
# analyze results from simulations
# first run p80
#--------------------------------------------------------------------------------------------------------------------------------------

analysis2 <- matrix(NA, nrow = length(run.scenarios) * length(or.gates) * length(os.gates), ncol = ncol(scenarios) + 6)
colnames(analysis2) <- c(colnames(scenarios), "CR gate", "OS gate", "power (no Fut)", "power (consider Fut)", "P(correct stop under H0)", "P(wrong stop under H1)")

iter <- 1

for (u in run.scenarios){
  
  # object sim.u
  load(file = paste(path, "02_OS_CR_interim/results/simul_results_scenario", u, sep = ""))
  
  # define simulation to be analyzed
  sim.in <- sim.u
  
  # H0 or H1?
  scen.u <- scenarios[u, ]
  h0 <- (scen.u["dat1"] == 0) & (scen.u["dat2"] == 0)
  h1 <- (scen.u["dat1"] == 0) & (scen.u["dat2"] == 1)
  
    for (g1 in 1:length(os.gates)){
      for (g2 in 1:length(or.gates)){
        
        os.gate.g <- min(Inf, os.gates[g1], na.rm = TRUE)
        analysis2[iter, "OS gate"] <- os.gates[g1]
        
        or.gate.g <- max(-Inf, or.gates[g2], na.rm = TRUE)
        analysis2[iter, "CR gate"] <- or.gates[g2]

        analysis2[iter, colnames(scenarios)] <- scen.u[colnames(scenarios)]
    
        # power not considering futility
        analysis2[iter, "power (no Fut)"] <- mean(sim.in$outcome1)

        # power considering futility
        survive.interim <- (sim.in$oddsratio1 >= or.gate.g) & (sim.in$hr1.os.int <= os.gate.g)
        analysis2[iter, "power (consider Fut)"] <- sum(sim.in$outcome1[survive.interim]) / scen.u["nsim"]

        # P(correctly stopping under H0)
        stop.interim <- (survive.interim == FALSE)
        if (h0){analysis2[iter, "P(correct stop under H0)"] <-  mean(stop.interim)}
  
        # P(wrongly stopping under H1)
        if (h1){analysis2[iter, "P(wrong stop under H1)"] <-  mean(stop.interim)}
  
        iter <- iter + 1

        }
      }
    }
  

# save results
save(analysis2, file = paste(path, "02_OS_CR_interim/results/analysis2", sep = ""))

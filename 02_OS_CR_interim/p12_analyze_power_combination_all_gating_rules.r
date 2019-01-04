#--------------------------------------------------------------------------------------------------------------------------------------
# analyze results from simulations
#--------------------------------------------------------------------------------------------------------------------------------------

# gate combinations to be used (NA means gate is not considered):
cr.gates1  <- 2.0
os.gates1  <- NA

cr.gates2  <- 2.5
os.gates2  <- NA

cr.gates3  <- NA
os.gates3  <- NA

analysis3 <- matrix(NA, nrow = length(run.scenarios) * length(cr.gates1), ncol = ncol(scenarios) + 12)
colnames(analysis3) <- c(colnames(scenarios), 
                         "CR gate 1", "OS gate 1",
                         "CR gate 2", "OS gate 2",
                         "CR gate 3", "OS gate 3",
                         "power (no Fut)", "power (consider Fut)", 
                         "conditional power", "P(correct stop under H0)", "P(wrong stop under H1)", 
                         "min OS HR at interim")

# collect OS HR at interim that lead to continuation
os.hr.int <- NULL

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
  
  for (g in 1:length(cr.gates1)){
  
    # set gates. If NA, then take a gate such that this endpoint is not relevant, i.e. this is then the univariate consideration.
    
    # gate 1
    cr.gate.g1 <- max(-Inf, cr.gates1[g], na.rm = TRUE)
    analysis3[iter, "CR gate 1"] <- cr.gates1[g]

    os.gate.g1 <- min(Inf, os.gates1[g], na.rm = TRUE)
    analysis3[iter, "OS gate 1"] <- os.gates1[g]

    # gate 2
    cr.gate.g2 <- max(-Inf, cr.gates2[g], na.rm = TRUE)
    analysis3[iter, "CR gate 2"] <- cr.gates2[g]

    os.gate.g2 <- min(Inf, os.gates2[g], na.rm = TRUE)
    analysis3[iter, "OS gate 2"] <- os.gates2[g]

    # gate 3
    cr.gate.g3 <- max(-Inf, cr.gates3[g], na.rm = TRUE)
    analysis3[iter, "CR gate 3"] <- cr.gates3[g]
    
    os.gate.g3 <- min(Inf, os.gates3[g], na.rm = TRUE)
    analysis3[iter, "OS gate 3"] <- os.gates3[g]

    analysis3[iter, colnames(scenarios)] <- scen.u[colnames(scenarios)]
    
    # power not considering futility
    analysis3[iter, "power (no Fut)"] <- mean(sim.in$outcome1)

    # power considering all the different gating rules
    # we continue if one of these rules is met
    rule1 <- (sim.in$oddsratio1 >= cr.gate.g1) & (sim.in$hr1.os.int <= os.gate.g1)
    rule2 <- (sim.in$oddsratio1 >= cr.gate.g2) & (sim.in$hr1.os.int <= os.gate.g2)
    rule3 <- (sim.in$oddsratio1 >= cr.gate.g3) & (sim.in$hr1.os.int <= os.gate.g3)
    survive.interim <- rule1 | rule2 | rule3
    analysis3[iter, "power (consider Fut)"] <- sum(sim.in$outcome1[survive.interim], na.rm = TRUE) / scen.u["nsim"]

    # conditional power    
    analysis3[iter, "conditional power"] <- sum(sim.in$outcome1[survive.interim], na.rm = TRUE) / sum(survive.interim)
    
    # P(correctly stopping under H0)
    stop.interim <- (survive.interim == FALSE)
    if (h0){analysis3[iter, "P(correct stop under H0)"] <-  mean(stop.interim, na.rm = TRUE)}
  
    # P(wrongly stopping under H1)
    if (h1){analysis3[iter, "P(wrong stop under H1)"] <-  mean(stop.interim)}
  
    # maximal OS HR at interim for which we continue
    analysis3[iter, "min OS HR at interim"] <- quantile(sim.in$hr1.os.int[survive.interim], prob = 0.95)
    
    iter <- iter + 1
  }
}


out3 <- analysis3[, c("scenario", "mut.rate", "N.interims", "power (no Fut)", "power (consider Fut)", "conditional power", "P(correct stop under H0)", "P(wrong stop under H1)", "min OS HR at interim")]
out3[, c("power (no Fut)", "power (consider Fut)", "conditional power", "P(correct stop under H0)", "P(wrong stop under H1)")] <- apply(out3[, c("power (no Fut)", "power (consider Fut)", "conditional power", "P(correct stop under H0)", "P(wrong stop under H1)")], 1:2, function(x){round(100 * x, 1)})
out3


plot(density(sim.in$hr1.os.int[survive.interim]))




#

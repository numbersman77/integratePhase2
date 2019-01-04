#--------------------------------------------------------------------------------------------------------------------------------------
# analyze final OS HR
#--------------------------------------------------------------------------------------------------------------------------------------

# gate combinations to be used (NA means gate is not considered):
efs.gates1 <- 1
cr.gates1  <- 2.0

efs.gates2 <- NA
cr.gates2  <- 2.5

# collect OS HRs at interim that lead to continuation
os.hr.int <- NULL

iter <- 1

# distribution of HR
par(las = 1)
plot(0, 0, xlim = c(0.3, 1.4), ylim = c(0, 2.5), type = "n", xlab = "hazard ratio", ylab = "density", main = "density estimates for OS HRs that passed interim")

for (u in run.scenarios){
  for (g in 1:length(efs.gates1)){
    
    # set gates. If NA, then take a gate such that this endpoint is not relevant, i.e. this is then the univariate consideration.
    
    # gate 1
    efs.gate.g1 <- min(Inf, efs.gates1[g], na.rm = TRUE)
    cr.gate.g1 <- max(-Inf, cr.gates1[g], na.rm = TRUE)
    
    # gate 2
    efs.gate.g2 <- min(Inf, efs.gates2[g], na.rm = TRUE)
    cr.gate.g2 <- max(-Inf, cr.gates2[g], na.rm = TRUE)
    
    # object sim.u
    load(file = paste(path, "simulations/02_OS_CR_interim/results/simul_results_scenario", u, sep = ""))
    
    # define simulation to be analyzed
    sim.in <- sim.u
    
    # H0 or H1?
    scen.u <- scenarios[u, ]
    
    # power considering all the different gating rules
    # we continue if one of these rules is met
    rule1 <- (sim.in$hr1.EFS <= efs.gate.g1) & (sim.in$oddsratio1 >= cr.gate.g1) 
    rule2 <- (sim.in$hr1.EFS <= efs.gate.g2) & (sim.in$oddsratio1 >= cr.gate.g2) 
    survive.interim <- rule1 | rule2
    
    # maximal OS HR at interim for which we continue
    os.hr.int[[iter]] <- sim.in$hr1.os.int[survive.interim]
    lines(density(os.hr.int[[iter]]), col = iter + 1)
    
    iter <- iter + 1
  }
}

lapply(os.hr.int, quantile, prob = c(0.8, 0.9, 0.95, 1))







#--------------------------------------------------------------------------------------------------------------------------------------
# analyze results from simulations
#--------------------------------------------------------------------------------------------------------------------------------------

analysis.nams <- c(colnames(scenarios), "Npts", "power", "trial duration (min)", 
                   "trial duration (median)", "trial duration (max)", "OS HR final (median)", 
                   "HR final 2.5th quantile", "HR final 25th quantile", "HR final 75th quantile", "HR final 97.5th quantile", 
                   "events final (ctrl, median)", "events final (tmt, median)", 
                   "median FU (final, median)", paste("power at t0 = ", t0s, sep = ""))
analysis1 <- matrix(NA, nrow = length(run.scenarios), ncol = length(analysis.nams))
colnames(analysis1) <- analysis.nams

iter <- 1

for (u in 1:length(run.scenarios)){
  
    scen.u <- scenarios[run.scenarios[iter] == scenarios[, "scenario"], ]
  
    # object res
    load(file = paste(path, "01_samplesize_OS/results/samplesizeOS_results_scenario", run.scenarios[u], sep = ""))
    analysis1[iter, colnames(scenarios)] <- scen.u[colnames(scenarios)]
    
    # average number of patients
    analysis1[iter, "Npts"] <- mean(res[, "n1"] + res[, "n2"])

    # power
    analysis1[iter, "power"] <- mean(res[, "p"] <= alpha)
    
    # trial duration
    analysis1[iter, c("trial duration (min)", "trial duration (median)", "trial duration (max)")] <- summary(res[, "cut"])[c("Min.", "Median", "Max.")]
    
    # OS HR at final
    analysis1[iter, "OS HR final (median)"] <- median(res[, "hr"])
    analysis1[iter, "HR final 2.5th quantile"] <- quantile(res[, "hr"], probs = 0.025)
    analysis1[iter, "HR final 25th quantile"] <- quantile(res[, "hr"], probs = 0.25)
    analysis1[iter, "HR final 75th quantile"] <- quantile(res[, "hr"], probs = 0.75)
    analysis1[iter, "HR final 97.5th quantile"] <- quantile(res[, "hr"], probs = 0.975)
    
    # median follow up at final
    analysis1[iter, "median FU (final, median)"] <- median(res[, "mfu"])
    
    # power at milestones
    analysis1[iter, paste("power at t0 = ", t0s, sep = "")] <- apply(res[, paste("landmark_", t0s, sep = "")] <= alpha, 2, sum, na.rm = TRUE) / scen.u["nsim"]
    
    iter <- iter + 1
}

analysis1


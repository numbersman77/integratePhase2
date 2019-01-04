#--------------------------------------------------------------------------------------------------------------------------------------
# analyze results from simulations
#--------------------------------------------------------------------------------------------------------------------------------------

analysis1_IA <- matrix(NA, nrow = length(run.scenarios), ncol = ncol(scenarios) + 22)
colnames(analysis1_IA) <- c(colnames(scenarios), "Npts", "power (no Fut)", "time to interim (median)", "trial duration (min)", "trial duration (median)", "trial duration (max)", 
                         "OS HR interim (median)", "OS HR final (median)", 
                         "OS KM at 30d (control, interim)", "OS KM at 30d (trt, interim)",
                         "OS 30d trt - ctrl (median)", "OS 30d trt - ctrl (10% quantile)", "OS 30d trt - ctrl (90% quantile)", 
                         "n of OS events (ctrl, interim)", "n of OS events (trt, interim)", "n of OS events (interim)", 
                         "n of OS events (ctrl, final)", "n of OS events (trt, final)", "n of OS events (final)", 
                         "HR OS CR (median)", "HR OS non-CR (median)", "median OS FU (interim)")

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
  
  analysis1_IA[iter, colnames(scenarios)] <- scen.u[colnames(scenarios)]
  
  # number of patients
  analysis1_IA[iter, "Npts"] <- sum(recruit * scen.u["recruit.prop"])
  
  # power not considering futility
  analysis1_IA[iter, "power (no Fut)"] <- mean(sim.in$outcome1)
  
  # time to interim
  analysis1_IA[iter, "time to interim (median)"] <- median(sim.in$trigger.interim)
  
  # trial duration
  analysis1_IA[iter, c("trial duration (min)", "trial duration (median)", "trial duration (max)")] <- summary(sim.in$trigger)[c("Min.", "Median", "Max.")]
  
  # OS HR at interim
  analysis1_IA[iter, "OS HR interim (median)"] <- median(sim.in$hr1.os.int)
  
  # OS HR at final
  analysis1_IA[iter, "OS HR final (median)"] <- median(sim.in$hr1)
  
  # height of KM estimate at 30 at interim
  analysis1_IA[iter, "OS KM at 30d (control, interim)"] <- median(sim.in$trt30d)
  analysis1_IA[iter, "OS KM at 30d (trt, interim)"]     <- median(sim.in$ctrl30d)
  
  # median OS difference at 30d at interim     
  d_OS <- sim.in$trt30d - sim.in$ctrl30d
  analysis1_IA[iter, "OS 30d trt - ctrl (10% quantile)"] <- quantile(d_OS, 0.1)
  analysis1_IA[iter, "OS 30d trt - ctrl (median)"]       <- median(d_OS)
  analysis1_IA[iter, "OS 30d trt - ctrl (90% quantile)"] <- quantile(d_OS, 0.9)
  
  # number of OS events at interim 
  analysis1_IA[iter, "n of OS events (ctrl, interim)"] <- median(sim.in$Nevents.os.int.ctrl)
  analysis1_IA[iter, "n of OS events (trt, interim)"]  <- median(sim.in$Nevents.os.int.trt)
  analysis1_IA[iter, "n of OS events (interim)"]  <- median(sim.in$Nevents.os.int.ctrl) + median(sim.in$Nevents.os.int.trt)
  
  # number of OS events at final 
  analysis1_IA[iter, "n of OS events (ctrl, final)"] <- median(sim.in$Nevents.os.ctrl)
  analysis1_IA[iter, "n of OS events (trt, final)"]  <- median(sim.in$Nevents.os.trt)
  analysis1_IA[iter, "n of OS events (final)"]  <- median(sim.in$Nevents.os.ctrl) + median(sim.in$Nevents.os.trt)
  
  # OS HR in CR and non-CR
  analysis1_IA[iter, "HR OS CR (median)"] <- median(sim.in$hr.cr)
  analysis1_IA[iter, "HR OS non-CR (median)"]  <- median(sim.in$hr.noncr)
  
  # median OS follow up at interim
  analysis1_IA[iter, "median OS FU (interim)"] <- median(sim.in$mFU1.os.int)
  
  iter <- iter + 1
}

analysis1_IA






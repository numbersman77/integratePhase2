#--------------------------------------------------------------------------------------------------------------------------------------
# simulations for OS sample size  
#--------------------------------------------------------------------------------------------------------------------------------------

nams <- c("hr", "p", "cut", "lam", "events.ctrl", "events.tmt", "median.FU")

iter <- 1
fails <- matrix(0, nrow = scen.u["nsim"], ncol = length(run.scenarios))

for (u in 1:length(run.scenarios)){  
  
  scen.u <- scenarios[run.scenarios[u] == scenarios[, "scenario"], ]
  
  # set up matrices to collect event times, treatment arm, and censoring indicator
  times <- matrix(NA, nrow = sum(pts), ncol = nsim)
  events <- times
  tmts <- times 
  hr <- rep(NA, scen.u["nsim"])
  p <- hr
  n1 <- hr
  n2 <- hr
  cut <- hr
  fu <- hr
  mspower <- matrix(NA, nrow = scen.u["nsim"], ncol = length(t0s))
  colnames(mspower) <- paste("landmark_", t0s, sep = "")
  
  set.seed(scen.u["seed"])
  
  for (j in 1:scen.u["nsim"]){
    
    # ----------------------------------
    # simulate recruitment
    # ----------------------------------
    pts.month <- rbinom(n = length(pts), size = pts, prob = pwt)
    pts.control <- rbinom(n = length(pts.month), size = pts.month, prob = rando.control)
    pts.tmt <- pts.month - pts.control
    recruit <- list(pts.control, pts.tmt)
    
    n1[j] <- sum(pts.control)
    n2[j] <- sum(pts.tmt)

    # ----------------------------------
    # simulate clinical trial
    # ----------------------------------
    time <- NA 
    while(all(is.na(time)) == TRUE){    
      # For scenario 2, in 4/100000 runs we do not seem to have enough events, because
      # random highs / lows for accrual, number of patients cured, and drop-out coincide. 
      # If that happens we simply regenerate the dataset.
    tab0 <- generateTimeToEventData2arm(scale = scen.u[c("scale.control", "scale.tmt")], 
                                        shape = scen.u[c("shape.control", "shape.tmt")], 
                                        cure = scen.u[c("cure.control", "cure.tmt")], 
                                        recruit = recruit, 
                                        dropout = scen.u[c("dropout.control", "dropout.tmt")], 
                                        start.accrual = c(0, 0), 
                                        cutoff = scen.u["d"], 
                                        seed = NA)
    tab <- tab0$tab
    time <- tab[, paste("time cutoff = ", scen.u["d"], sep = "")]
    event <- tab[, paste("event cutoff = ", scen.u["d"], sep = "")]
    tmt <- tab[, "tmt"]
    fails[j, u] <- fails[j, u] + 1
    }
    
    # ----------------------------------
    # remove patients that arrived after cutoff
    # ----------------------------------
    rem <- (is.na(time) == FALSE)
    time <- time[rem]
    event <- event[rem]
    tmt <- tmt[rem]
    
    # ----------------------------------
    # save data
    # ----------------------------------
    times[1:length(time), j] <- time
    events[1:length(event), j] <- event
    tmts[1:length(tmt), j] <- tmt
    
    # ----------------------------------------------------------------------
    # hazard ratio
    # ----------------------------------------------------------------------
    cox1 <- summary(coxph(Surv(times[, j], events[, j]) ~ tmts[, j]))
    hr[j] <- exp(coef(cox1)[1])
    
    # ----------------------------------------------------------------------
    # logrank p-value
    # ----------------------------------------------------------------------
    p[j] <- coef(cox1)[5]
    
    # ----------------------------------------------------------------------
    # final analysis cutoff
    # ----------------------------------------------------------------------
    cut[j] <- tab0$cutoff.time

    # ----------------------------------------------------------------------
    # median FU at final
    # ----------------------------------------------------------------------
    mfu <- quantile(survfit(Surv(time, event == 0) ~ rep(0, length(time))), probs = 0.5)$quantile
    fu[j] <- as.numeric(mfu)

    # ----------------------------------
    # compare arms @ given milestone times
    # ----------------------------------
    for (k in 1:length(t0s)){
      t0 <- t0s[k]
      mspower[j, k] <- landmarkTest(time, event, tmt, t0)
    }

    if (j / 10 ^ 3 == round(j / 10 ^ 3)){print(paste("run ", j, " of ", format(scen.u["nsim"], scientific = FALSE), " in scenario ", iter, " of ", length(run.scenarios), " done", sep = ""))}
  }  
  
  iter <- iter + 1
  
  # save results
  res <- cbind("hr" = hr, "p" = p, "n1" = n1, "n2" = n2, "cut" = cut, "mfu" = mfu, mspower)
  save(res, file = paste(path, "01_samplesize_OS/results/samplesizeOS_results_scenario", run.scenarios[u], sep = ""))
}

save(fails, file = paste(path, "01_samplesize_OS/results/fails", sep = ""))


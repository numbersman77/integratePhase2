generateStudy_OS <- function(study.info){   
     
     # 2:1 randomization
     rand.list.stage1 <- as.vector(apply(matrix(rep(c(1, 1, 2, 2, 2, 2), ceiling(study.info$Npt / 6)), 6, ceiling(study.info$Npt / 6)), 2, sample)) 
     rand.list.stage2 <- as.vector(apply(matrix(rep(c(1, 1, 2, 2, 2, 2), ceiling(study.info$Npt / 6)), 6, ceiling(study.info$Npt / 6)), 2, sample)) 
     rand.list <- c(rand.list.stage1[1:study.info$N.interim], rand.list.stage2[(study.info$N.interim + 1):study.info$Npt])
     stages <- c(rep(1, study.info$N.interim), rep(2, study.info$Npt - study.info$N.interim))
     
     # cut recruitment at Npt
     c.rec <- cumsum(study.info$recruit)
     rec <- study.info$recruit[c.rec < study.info$Npt]
     rec <- c(rec, study.info$Npt - tail(cumsum(rec), 1))
     
     # Patients enrolled according to a Poisson process with average: 
     # Study.info$recruit patients per month --> inter arrival times ~EXP(lambda = 1 / pat per month)
     # Cumulative enrolment time since only inter-arrival times simulated above
     enrol.time <- NULL
     for (o in 1:length(rec)){
       enrol.time.o <- -log(runif(rec[o])) / rec[o]
       for(i in 2:rec[o]){enrol.time.o[i] <- enrol.time.o[i] + enrol.time.o[i - 1]}
       enrol.time <- c(enrol.time, enrol.time.o + (o - 1))
     }
     
     # ----------------- Generate vectors used in simulation ---------------------------------------------------------------
     # Time to event (time to event or censoring)
     event.time <- rep(-1, study.info$Npt) 

     # Time to event (time to event or censoring)
     event.time.int <- rep(-1, study.info$Npt) 

     # Study time of event or censoring (event.time + enroll.time)
     study.time <- rep(-1, study.info$Npt) 

     # Time to loss to follow up
     cens.time <- rep(-1, study.info$Npt) 

     # Assign randomly treatment from randomization list (1 = H0 Comparator, 2 = H1 MDM2(4))
     trt <- rand.list[1:study.info$Npt]  

     # Captures censoring (1 = had event, 0 = censored), used for variable trigger
     event <- rep(-1, study.info$Npt) 

     # p53 Mutation status for patients in trial; 0 = p53 wild-type, 1 = mutation positive (i.e. MDM2(4) not working)
     mut.status <- rep(-1, study.info$Npt) 

     # CR status
     cr.status <- rep(-1, study.info$Npt) 

     # to randomly draw a line of the big GenData0/1 data matrix  
     select1 <- sample(c(1:nrow(study.info$store0)), study.info$Npt, replace = TRUE)  
     
     for(i in 1:study.info$Npt){
       
       # if patient receives Ara-C single agent
       if((rand.list[i] == 1)){
         sel.control <- select1[i]
         col.store0 <- study.info$store0[sel.control, 1:3]
         event.time[i] <- col.store0["CR.status OS"]
         mut.status[i] <- col.store0["mut.status"]    
         cr.status[i] <- col.store0["CR.status"]
        } 
       
       # 2.Patient receives MDM2(4) 
       if(rand.list[i] == 2){   
            sel.H1 <- select1[i]
            col.store1 <- study.info$store1[sel.H1, 1:3]
            event.time[i] <- col.store1["CR.status OS"]
            mut.status[i] <- col.store1["mut.status"]  
            cr.status[i] <- col.store1["CR.status"]
       }  
       
       study.time[i] <- enrol.time[i] + event.time[i]

       # Apply censoring risk (drop-out according to Poisson process)
       cens.time[i] <- enrol.time[i] + rexp(1, study.info$dropout.month) 
        
       # drop.out
       if(study.time[i] <= cens.time[i]){event[i] <- 1} else {event[i] <- 0} 
     } # end for i
     
     # trigger for final analysis of OS (including cut-off if # events cannot be reached) 
     trigger <- sort(study.time[event == 1])[min(study.info$Nev, length(study.time[event == 1]))] 

     # interim analysis after N.interim patients 
     trigger.interim <- enrol.time[study.info$N.interim] 
     
     # prepare event times for censoring at interim
     event.time.int <- event.time
     
     # -----------------------------------------
     # 1a) censoring for OS done for final analysis
     # -----------------------------------------
     
     # 1 = event observed, 0 = censored
     censor <- rep(-1, study.info$Npt) 

     # Apply censoring at the time of analysis trigger
     for(i in 1:study.info$Npt){   
       
       # Event is observed (event is prior to trigger AND prior to random censoring)
       if((study.time[i] <= trigger) && (study.time[i] <= cens.time[i])){censor[i] <- 1} else {
       
       # Event is randomly censored prior to analysis trigger
          if(cens.time[i] <= trigger){   
            censor[i] <- 0
            event.time[i] <- cens.time[i] - enrol.time[i]
       # Event occurs after analysis trigger/cut-off         
          } else {    
             censor[i] <- 0
             event.time[i] <- trigger - enrol.time[i]  
         }
       }
     } # end for i
     

     # -----------------------------------------
     # 1b) OS censored at interim analysis --> to know the OS status at interim
     # -----------------------------------------
     
     censor.int <- rep(-1, study.info$Npt) 
     
     # Apply censoring at the time of interim analysis trigger (trigger.interim)
     for(i in 1:study.info$Npt){
       
       # Event is observed (event is prior to trigger AND prior to random censoring)
       if((study.time[i] <= trigger.interim) && (study.time[i] <= cens.time[i])){censor.int[i] <- 1} else {
       
       # Event is randomly censored prior to interim analysis trigger (trigger.interim)
         if(cens.time[i] <= trigger.interim){
          censor.int[i] <- 0
          event.time.int[i] <- cens.time[i]
         
         # Event occurs after interim analysis trigger (trigger.interim)         
       } else{censor.int[i] <- 0
              event.time.int[i] <- trigger.interim - enrol.time[i]  
       }
       }
     } # end for i
     
          
     ##-------------------------------------------------------------------------
     ## Adjust output to sample of patients from interim analysis 
     ##-------------------------------------------------------------------------
     
     # "True" if patient is part of the interim analysis at time point = trigger
     id.pat.interim <- (enrol.time <= trigger) 
     
     # Number of OS events observed at final analysis
     Nevents1 <- sum(censor)         

     # Number of OS events at the interim
     Nevents.os.int.ctrl <- sum(censor.int[trt == 1][stages == 1], na.rm = TRUE)
     Nevents.os.int.trt <- sum(censor.int[trt == 2][stages == 1], na.rm = TRUE)
     
     # Number of OS events at the final
     Nevents.os.ctrl <- sum(censor[trt == 1])
     Nevents.os.trt <- sum(censor[trt == 2])

     # Compute also Odds ratio for using as futility criteria  
     study <- list(event.time = event.time,        # Time to event (from enrolment to event or censoring)
                   event.time.int = event.time.int[stages == 1],  # OS at interim
                   study.time = study.time,        # Study time of event or censoring (event.time+enroll.time)
                   censor     = censor,            # OS censor; 1 = event observed, 0 = censored
                   cens.time  = cens.time[id.pat.interim == TRUE],         # Time to loss to follow up
                   censor.int = censor.int[stages == 1],   # OS censoring indicator at interim
                   event      = event,             
                   trt        = trt,               # Treatment (1=H0 Comparator, 2=H1 MDM2(4))
                   trt.int    = trt[stages == 1],   # trt status at interim, for OS at interim analysis
                   trigger    = trigger,             # Min(last of pre-specified # OS events OR #events observed)
                   trigger.interim = trigger.interim,# Trigger for interim analysis
                   Nevents1   = Nevents1,            # Number of OS events observed at final analysis
                   mut.status = mut.status,            # Returns mutation status for all patients in the trial (eg useful to perform sensitivity analyses around mutation)
                   cr.status = cr.status,            # Returns CR status
                   stages     = stages,            # indicates stages 1 and 2 for each patient
                   Nevents.os.int.ctrl = Nevents.os.int.ctrl,
                   Nevents.os.int.trt  = Nevents.os.int.trt,
                   Nevents.os.ctrl = Nevents.os.ctrl,
                   Nevents.os.trt  = Nevents.os.trt                   
     )
     
     return(study)
}











#

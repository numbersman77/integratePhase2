simulate_OS <- function(study.info, nsim = 10000, print.freq = 10, alpha = 0.05){   

    empty <- rep(-1, nsim)
  
    outcome1        <- empty
    outcome1.wild   <- empty
    hr1             <- empty
    hr1.wild        <- empty
    med11           <- empty
    med12           <- empty
    trigger         <- empty
    trigger.interim <- empty
    Nevents1        <- empty
    med11.wild      <- empty
    med12.wild      <- empty
    p.prophaztest   <- empty
    oddsratio1      <- empty 
    ctrl30d         <- empty
    trt30d          <- empty
    hr1.os.int      <- empty
    Nevents.os.int.ctrl <- empty
    Nevents.os.int.trt  <- empty
    Nevents.os.ctrl <- empty
    Nevents.os.trt  <- empty
    hr.cr           <- empty
    hr.noncr        <- empty
    hr.diff         <- empty
    
     for(k in 1:nsim){   
        study <- generateStudy_OS(study.info)
        trigger[k] <- study$trigger
        trigger.interim[k] <- study$trigger.interim
        Nevents1[k] <- study$Nevents1
 
        # Analyze study
        tmp <- analyse_OS(study, alpha = alpha)  
         
        # outcome = 0 if study negative
        # if p-value <= 0.05 then outcome = 1
        outcome1[k] <- tmp[1]       
        hr1[k]<- tmp[2]
        med11[k] <- tmp[3]
        med12[k] <- tmp[4]
        outcome1.wild[k] <- tmp[5]    
        hr1.wild[k] <- tmp[6]
        med11.wild[k] <- tmp[7]
        med12.wild[k] <- tmp[8]
        p.prophaztest[k] <- tmp[9]
        oddsratio1[k] <- tmp[10]
        
        # OS difference from KM at interim @ 30d
        ctrl30d[k] <- tmp[11]
        trt30d[k]  <- tmp[12]
        
        hr1.os.int[k] <- tmp[13]
        Nevents.os.int.ctrl[k] <- tmp[14]
        Nevents.os.int.trt[k] <- tmp[15]
      
        Nevents.os.ctrl[k] <- tmp[16]
        Nevents.os.trt[k] <- tmp[17]

        # OS HR @ final, per CR vs. non-CR
        hr.cr[k] <- tmp[18]
        hr.noncr[k] <- tmp[19]
        hr.diff[k] <- hr.noncr[k] - hr.cr[k]
        c(hr1[k], hr.cr[k], hr.noncr[k])
         
        # median OS FU at interim
        mFU1.os.int <- tmp[20]
        
        if (k / print.freq == round(k / print.freq)){print(paste("simulation run ", k, " of ", nsim, " done", sep = ""))}
     }  # end for k
     
     # prepare return object
     store <- list(outcome1        = outcome1,            
                   hr1             = hr1,
                   med11           = med11,
                   med12           = med12,
                   trigger         = trigger,
                   Nevents1        = Nevents1,
                   trigger.interim = trigger.interim,
                   outcome1.wild   = outcome1.wild,  
                   hr1.wild        = hr1.wild,
                   med11.wild      = med11.wild,
                   med12.wild      = med12.wild,
                   p.prophaztest   = p.prophaztest,
                   oddsratio1      = oddsratio1,
                   ctrl30d         = ctrl30d,
                   trt30d          = trt30d,
                   hr1.os.int      = hr1.os.int,
                   Nevents.os.int.ctrl = Nevents.os.int.ctrl,
                   Nevents.os.int.trt  = Nevents.os.int.trt,
                   Nevents.os.ctrl = Nevents.os.ctrl,
                   Nevents.os.trt  = Nevents.os.trt,
                   hr.cr           = hr.cr,
                   hr.noncr        = hr.noncr,
                   hr.diff         = hr.diff,
                   mFU1.os.int     = mFU1.os.int
     )
     
     return(store)
}

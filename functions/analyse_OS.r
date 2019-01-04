analyse_OS <- function(study, alpha = 0.05){   

     # Analyze study generation outcome
     tme <- study$event.time       
     cens <- study$censor
     trt <- study$trt
     mut.status <- study$mut.status
     cr.status <- study$cr.status
     stages <- study$stages
     
     ## odds ratio for CR at interim
     ind <- (stages == 1)
     cr.status1 <- cr.status[ind]
     trt1 <- trt[ind]
     pCRtmt <- mean(cr.status1[trt1 == 2] == 0)
     pCRcontrol <- mean(cr.status1[trt1 == 1] == 0)
     oddsratio1 <- (pCRtmt / (1 - pCRtmt)) / (pCRcontrol / (1 - pCRcontrol))
     
     ## Compute survival objects
     surv.OS <- Surv(tme, cens == 1)

     # Only wild-type patients
     tme.wild <- tme[mut.status == 0]   
     cens.wild <- cens[mut.status == 0]
     trt.wild <- trt[mut.status == 0]
     surv.wild <- Surv(tme.wild, cens.wild==1)
     
     res1 <- survdiff(surv.OS ~ trt)
     res1.wild <- survdiff(surv.wild ~ trt.wild)
    
     # other test statistics
     cph1 <- coxph(surv.OS ~ trt)

     # test proportional hazard assumption
     test.PH <- cox.zph(cph1, transform = "km", global = TRUE)
     #p.prophaztest=print(test.PH)[3]
     p.prophaztest <- test.PH$table[3]
 
     # hazard ratio for OS
     hr1 <- exp(cph1$coefficients) 

     # hazard ratio for OS in CR patients only
     cph2 <- coxph(surv.OS ~ trt, subset = (cr.status == 0))
     hr.cr <- exp(cph2$coefficients) 
     
     # hazard ratio for OS in CR patients only
     cph3 <- coxph(surv.OS ~ trt, subset = (cr.status == 1))
     hr.noncr <- exp(cph3$coefficients) 
     
     # hazard ratio for OS in wild-type patients
     cph1.wild <- coxph(surv.wild ~ trt.wild)
     hr1.wild <- exp(cph1.wild$coefficients) 
     med.hp <- summary(survfit(surv.OS ~ trt)) 
     med11 <- med.hp$table[1, "median"]
     med12 <- med.hp$table[2, "median"]
     med.wild.hp <- summary(survfit(surv.wild ~ trt.wild))
     med11.wild <- med.wild.hp$table[1, "median"]
     med12.wild <- med.wild.hp$table[2, "median"]
     
     # OS difference at 30d @ interim
     # plot(survfit(Surv(study$event.time.int, study$censor.int)~study$trt.int), col = 2:3)
     ctrl30d <- confIntKM_t0(time = study$event.time.int[study$trt.int == 1], event = study$censor.int[study$trt.int == 1], t0 = 1)
     trt30d <- confIntKM_t0(time = study$event.time.int[study$trt.int == 2], event = study$censor.int[study$trt.int == 2], t0 = 1)

     # OS HR at interim
     cph1.os.int <- coxph(Surv(study$event.time.int, study$censor.int) ~ study$trt.int)
     hr1.os.int <- exp(cph1.os.int$coefficients) 
     
     # median follow up at interim
     mFU1.os.int <- as.numeric(quantile(survfit(Surv(study$event.time.int, study$censor.int == 0) ~ rep(1, length(study$trt.int))), probs = 0.5)$quantile)
     
     # Compute indicator varaible for positive / negative study outcome for OS (overall and wild-type population)
     # outcome1 = 0 if study negative for OS overall
     # if p-value <= alpha than outcome = 1
     if (1 - pchisq(res1$chisq, 1) <= alpha){outcome1 <- 1} else {outcome1 <- 0}
     
     # outcome1.wild = 0 if study negative for OS in wild-type patients
     # if p-value <= alpha than outcome1.wild = 1
     if (1 - pchisq(res1.wild$chisq, 1) <= alpha){outcome1.wild <- 1}
     else outcome1.wild = 0
     
     # output
     res <- c(outcome1, hr1, med11, med12, outcome1.wild, hr1.wild, med11.wild, med12.wild, p.prophaztest, oddsratio1, 
              ctrl30d[2], trt30d[2], hr1.os.int, study$Nevents.os.int.ctrl, study$Nevents.os.int.trt, study$Nevents.os.ctrl, study$Nevents.os.trt, 
              hr.cr, hr.noncr, mFU1.os.int)
     return(res)
}

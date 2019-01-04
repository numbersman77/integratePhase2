generateData <- function(p0 = prob.CR, p1.corr = p1.corr, HR.OS = 0.72, mut.rate = mut.rate, prob.longterm = 0.5, 
                         med.nonresp.H0 = 5.5, med.resp.shortterm.H0 = 7, long.surv = 240, n = 500000){   
     
     # mutation status: 0 = p53 wild-type, 1 = mutation positive (i.e. MDM2(4) not working)     
     mut.status <- sample(c(0, 1), n, prob = c(1 - mut.rate, mut.rate), replace = TRUE) 
     N.mut <- sum(mut.status)
     N.nonmut <- n - N.mut

     # CR status: 0 = CR, 1 = non-CR
     CR.status <- c(sample(c(0, 1), N.nonmut, prob = c(p1.corr, (1 - p1.corr)), replace = TRUE), sample(c(0, 1), N.mut, prob = c(p0, 1 - p0), replace = TRUE))
    
     # 50%/50% chance to be short term versus long term survivor 
     # (based on 40% transplant and 10% long-term without transplant)
     # generated for all patients, but only used for CRs (ease of computation)
     longterm <- rbinom(n, size = 1, prob = prob.longterm)  
     #test: 1 - mean(CR.status) #should be approx 21.7
       
     # generate output:
     dat <- matrix(NA, nrow = n, ncol = 3)
     colnames(dat) <- c("CR.status OS", "mut.status", "CR.status")
             
     # Assignment of survival distributions to different "cohorts"
     ind.nonresp.H0    <- (CR.status == 1)                   & (mut.status == 1)
     ind.nonresp.H1    <- (CR.status == 1)                   & (mut.status == 0)
     
     ind.resp.longterm <- (CR.status == 0) & (longterm == 1)

     ind.short.H0      <- (CR.status == 0) & (longterm == 0) & (mut.status == 1) 
     ind.short.H1      <- (CR.status == 0) & (longterm == 0) & (mut.status == 0) 
    
     N.nonresp.H0 <- sum(ind.nonresp.H0) 
     N.nonresp.H1 <- sum(ind.nonresp.H1)
     N.short.H0 <- sum(ind.short.H0)
     N.short.H1 <- sum(ind.short.H1)
     
     # ------------------------------------------------- 
     # a) Survival assignment for CR non-responders (one for mutants and one for wild-types)
    
     # OS: non-responders ~Exp with median OS received via HR.OS
     lam.nonresp.H0 <- log(2) / med.nonresp.H0
     lam.nonresp.H1 <- log(2) / (med.nonresp.H0 / HR.OS)
     
     dat[ind.nonresp.H0, "CR.status OS"] <- rexp(N.nonresp.H0, rate = lam.nonresp.H0)  
     dat[ind.nonresp.H1, "CR.status OS"] <- rexp(N.nonresp.H1, rate = lam.nonresp.H1)  
     # -------------------------------------------------
    
     # -------------------------------------------------
     # b) Survival assignment for CR short-term responders (H0 resp H1)
     # OS (HR similar than for non-responders above) 
     lam.resp.shortterm.H0 <- log(2) / med.resp.shortterm.H0
     lam.resp.shortterm.H1 <- log(2) / (med.resp.shortterm.H0 / HR.OS)
     
     dat[ind.short.H0, "CR.status OS"] <- rexp(N.short.H0, rate = lam.resp.shortterm.H0)  
     dat[ind.short.H1, "CR.status OS"] <- rexp(N.short.H1, rate = lam.resp.shortterm.H1) 
     # -------------------------------------------------
    
     # -------------------------------------------------
     # c) Survival assignment for CR long-term responders (H0 resp H1, same value assigned)
     # arbitrary 240 months survival for lon-term survivor ("cure fraction patients")  
     # OS
     dat[ind.resp.longterm, "CR.status OS"] <- long.surv 
     # -------------------------------------------------
    
     dat[, "mut.status"] <- mut.status
     dat[, "CR.status"] <- CR.status
     
     return(dat) 
}  






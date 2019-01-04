#--------------------------------------------------------------------------------------------------------------------------------------
# Define scenarios for simulations with interim analysis   
#
# Description of variables:
#
# p1.cr: CR status. 0 = CR, 1 = non-CR.
# mut.rate: Mutation rate. 
# OR.CR: Odds ratio by which tmt has a better response than standard.
# Npts: trial sample size
# Nevs: number of events when final analysis should take place
# N.interims: number of events at the interim
# dat1: indicates dataset 1 for comparison
# dat2: indicates dataset 2 for comparison
#       If dat1 = dat2 --> analyze H0, otherwise compare dat1 = dat2.
#--------------------------------------------------------------------------------------------------------------------------------------

scenarios <- rbind(
  c(1,  p1.cr, mut.rate, 0.85, OR.CR, HR.OS, 275, 120, 0.05, 0, 0, nsim),
  c(2,  p1.cr, mut.rate, 0.85, OR.CR, HR.OS, 275, 120, 0.05, 0, 1, nsim)
)

scenarios[, 1] <- 1:nrow(scenarios)
colnames(scenarios) <- c(
  "scenario", "p1.cr", "mut.rate", "recruit.prop", 
  "OR.CR", "HR.OS", "Nevs", "N.interims", 
  "alpha", "dat1", "dat2", "nsim")







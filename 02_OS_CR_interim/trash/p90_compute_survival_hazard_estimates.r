# ---------------------------------------------------------------------------------
# compute Kaplan-Meier estimates based on enough simulated OS and EFS times
# ---------------------------------------------------------------------------------
time.os.control  <- GenData0[, 1]
time.os.tmt      <- GenData1[, 1]
time.efs.control <- GenData0[, 2]
time.efs.tmt     <- GenData1[, 2]
os.control       <- survfit(Surv(time.os.control) ~ 1)
os.tmt           <- survfit(Surv(time.os.tmt) ~ 1)
efs.control      <- survfit(Surv(time.efs.control) ~ 1)
efs.tmt          <- survfit(Surv(time.efs.tmt) ~ 1)


# ---------------------------------------------------------------------------------
# hazard functions
# smoothness is enough for subset of simulated event times
# ---------------------------------------------------------------------------------
myhaz <- function(dat){muhaz(dat[1:100000], min.time = 0, max.time = 60, n.min.grid = 100, n.est.grid = 1000, bw.method = "l")}
haz.os.control  <- myhaz(time.os.control)
haz.os.tmt      <- myhaz(time.os.tmt)
haz.efs.control <- myhaz(time.efs.control)
haz.efs.tmt     <- myhaz(time.efs.tmt)















#
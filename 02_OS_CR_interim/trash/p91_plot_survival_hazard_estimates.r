# ---------------------------------------------------------------------------------
# should efs be added to plots?
# ---------------------------------------------------------------------------------
add.efs <- TRUE
if (add.efs == TRUE){
  select <- 1:6
  select2 <- 1:2} else {
    select <- c(1:2, 5:6)
    select2 <- 1
  }

# ---------------------------------------------------------------------------------
# basic parameters
# ---------------------------------------------------------------------------------
leg <- c("OS MDM2(4) + Cytarabine","OS Cytarabine","EFS MDM2(4) + Cytarabine","EFS Cytarabine", "Cytarabine + MDM2(4) cure proportion", "Cytarabine cure proportion")
cols <- c("red", "red", "blue", "blue", "darkgreen", "purple")
ltys <- c(1, 2, 1, 2, 3, 3)

# cure proportions --> look at curves at 240 months
m <- 239.9

# OS, cytarabine
cure.control <- min(os.control$surv[os.control$time <= m])

# OS, combo
cure.tmt <- min(os.tmt$surv[os.tmt$time <= m])

file <- paste(path.results, "survival_estimates", sep = "")
jpeg(paste(file, ".jpg", sep = ""), width = 1270, height = 900, quality = 2000, units = "px", pointsize = 20, bg = "white")
par(mar = c(4.5, 4.5, 4, 1), las = 1, oma = rep(0, 4))
plot(0, 0, type = "n", xlim = c(1.5, 50), ylim = c(0, 1), xlab = "Time (months)", ylab = "proportion surviving", main = "survival functions from simulation model")
abline(h = 0, col = grey(0.25))
mtext("Kaplan-Meier estimates based on a sufficient number of simulations", side = 3, line = 0.3)

lines(os.tmt, col = cols[1], lwd = 3, lty = ltys[1])
lines(os.control, col = cols[2], lwd = 3, lty = ltys[2])
if (add.efs){
  lines(efs.tmt, col = cols[3], lwd = 3, lty = ltys[3])
  lines(efs.control, col = cols[4], lwd = 3, lty = ltys[4])
}
abline(h = cure.tmt, lty = ltys[5], col = cols[5], lwd = 3)
abline(h = cure.control, lty = ltys[6], col = cols[6], lwd = 3)

# add parametric cure model for illustration
med.tmt <- 8
xs <- seq(0, 100, by = 0.01)
m <- 9
lam0 <- (- log((0.5 - cure.tmt) / (1 - cure.tmt)) / m)
Stmt <- cure.tmt + (1 - cure.tmt) * (1 - pexp(xs, rate = lam0))
lines(xs, Stmt, col = "green")

legend("topright", legend = leg[select], lwd = 3, col = cols[select], lty = ltys[select], bty = "n")
dev.off()
graphics.off()



# ---------------------------------------------------------------------------------
# hazard functions
# ---------------------------------------------------------------------------------
file <- paste(path.results, "hazard_estimates", sep = "")
jpeg(paste(file, ".jpg", sep = ""), width = 1270, height = 900, quality = 2000, units = "px", pointsize = 20, bg = "white")
par(mar = c(4.5, 4.5, 4, 1), las = 1, oma = rep(0, 4))
plot(0, 0, type = "n", xlim = c(1.5, 50), ylim = c(0, 0.3), xlab = "Time (months)", ylab = "hazard", main = "hazard functions from simulation model")
abline(h = 0, col = grey(0.25))
mtext("Kernel estimates of hazard functions, based on a sufficient number of simulations", side = 3, line = 0.3)

lines(haz.os.tmt, col = cols[1], lwd = 3, lty = ltys[1])
lines(haz.os.control, col = cols[2], lwd = 3, lty = ltys[2])
if (add.efs){
  lines(haz.efs.tmt, col = cols[3], lwd = 3, lty = ltys[3])
  lines(haz.efs.control, col = cols[4], lwd = 3, lty = ltys[4])
}
legend("topright", legend = leg[1:4][select[select <= 4]], lwd = 3, col = cols[1:4][select[select <= 4]], lty = ltys[1:4][select[select <= 4]], bty = "n")
dev.off()
graphics.off()




# ---------------------------------------------------------------------------------
# hazard functions and ratio of hazard functions --> check proportionality
# smoothness is enough for subset of simulated event times
# ---------------------------------------------------------------------------------
file <- paste(path.results, "hazard_estimates_ratios", sep = "")
jpeg(paste(file, ".jpg", sep = ""), width = 1400, height = 900, quality = 2000, units = "px", pointsize = 20, bg = "white")
par(mar = c(4.5, 4.5, 3, 1), las = 1, oma = c(0, 0, 0, 0), mfrow = c(1, 3))

# estimates of survival functions
plot(0, 0, type = "n", xlim = c(1.5, 50), ylim = c(0, 1), xlab = "Time (months)", ylab = "proportion surviving", 
     main = "survival functions")
abline(h = 0, col = grey(0.25))

lines(os.tmt, col = cols[1], lwd = 3, lty = ltys[1])
lines(os.control, col = cols[2], lwd = 3, lty = ltys[2])
if (add.efs){
  lines(efs.tmt, col = cols[3], lwd = 3, lty = ltys[3])
  lines(efs.control, col = cols[4], lwd = 3, lty = ltys[4])
}
    
abline(h = cure.tmt, lty = ltys[5], col = cols[5], lwd = 3)
abline(h = cure.control, lty = ltys[6], col = cols[6], lwd = 3)

legend("topright", legend = leg[select], lwd = 3, col = cols[select], lty = ltys[select], bty = "n")


# estimates of hazard functions
plot(0, 0, type = "n", xlim = c(1.5, 50), ylim = c(0, 0.3), xlab = "Time (months)", ylab = "hazard", main = "hazard functions")
abline(h = 0, col = grey(0.25))

lines(haz.os.tmt, col = cols[1], lwd = 3, lty = ltys[1])
lines(haz.os.control, col = cols[2], lwd = 3, lty = ltys[2])

if (add.efs){
  lines(haz.efs.tmt, col = cols[3], lwd = 3, lty = ltys[3])
  lines(haz.efs.control, col = cols[4], lwd = 3, lty = ltys[4])
}
legend("topright", legend = leg[1:4][select[select <= 4]], lwd = 3, col = cols[1:4][select[select <= 4]], lty = ltys[1:4][select[select <= 4]], bty = "n")

#title("estimates from simulation output, based on a sufficient number of simulations", outer = TRUE)

# ratios, for OS and EFS
plot(0, 0, type = "n", xlim = c(1.5, 50), ylim = c(0, 3), xlab = "Time (months)", ylab = "ratio of hazard functions", 
     main = "ratio of hazard functions")
abline(h = c(0, 1), col = grey(0.25))

# check whether grid is ok:
range(haz.os.tmt$est.grid - haz.os.control$est.grid)
range(haz.efs.tmt$est.grid - haz.efs.control$est.grid)

lines(haz.os.tmt$est.grid, haz.os.tmt$haz.est / haz.os.control$haz.est, col = cols[1], lwd = 3, lty = ltys[1])
if (add.efs){
  lines(haz.efs.tmt$est.grid, haz.efs.tmt$haz.est / haz.os.control$haz.est, col = cols[3], lwd = 3, lty = ltys[1])
}
legend("topright", legend = paste(c("OS", "EFS"), "MDM2(4) + Cytarabine vs. Cytarabine alone", sep = "")[select2], lwd = 3, col = cols[c(1, 3)][select2], lty = ltys[1], bty = "n")

dev.off()
graphics.off()















#
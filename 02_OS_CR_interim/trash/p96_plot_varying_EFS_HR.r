# ---------------------------------------------------------------------------------
# plot rejection probabilities for varying EFS HR
# ---------------------------------------------------------------------------------

file <- paste(path.results, "varying_EFS_HR", sep = "")
jpeg(paste(file, ".jpg", sep = ""), width = 1270, height = 900, quality = 2000, units = "px", pointsize = 20, bg = "white")
par(mar = c(4.5, 4.5, 4, 1), las = 1, oma = rep(0, 4))

plot(0, 0, type = "n", xlim = range(analysis[, "HR.EFS"]), ylim = c(0, 1), xlab = "EFS hazard ratio", ylab = "P(stop @ interim | H1 is true)", main = "wrong stopping probabilities for varying EFS HR")
mtext("Kaplan-Meier estimates based on a sufficient number of simulations", side = 3, line = 0.3)

lines(analysis[, "HR.EFS"], analysis[, "P(wrong stop under H1)"], type = "b", col = "red")

legend("topright", legend = leg[select], lwd = 3, col = cols[select], lty = ltys[select], bty = "n")
dev.off()
graphics.off()






out1 <- analysis[, c("scenario", "mut.rate", "factor.medOS.medEFS.H0", "HR.OS", "HR.EFS", "Corr.EFSOS", "Npts", "Nevs", "N.interims", "dat1", "dat2", "OR.CR", "EFS gate 1", "OR gate 1", "EFS gate 2", "OR gate 2", 
                     "power (no Fut)", "power (consider Fut)", "P(correct stop under H0)", "P(wrong stop under H1)")]
data.frame(out1)





#
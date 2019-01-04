# ---------------------------------------------------------------------------------
# first run p81
# ---------------------------------------------------------------------------------

u.or <- unique(analysis2[, "CR gate"])

file <- paste(path.results, "vary_gates", sep = "")
jpeg(paste(file, ".jpg", sep = ""), width = 1270, height = 900, quality = 2000, units = "px", pointsize = 20, bg = "white")
par(mar = c(4.5, 4.5, 4, 1), mfrow = c(1, length(u.or)), las = 1, oma = rep(0, 4))

# plot for OR gate = 2.25

for (j in 1:length(u.or)){
  plot(0, 0, type = "n", xlim = range(v1), ylim = c(0, 0.4), xaxt = "n", xlab = "EFS gate", ylab = "P(stop @ interim | H1 is true)", main = "")
  mtext(paste("OR direct gate = ", u.or[j], sep = ""), side = 3, line = 0.3)
  axis(1, at = v1, labels = v1)
  
  ind1 <- ((analysis[, "OR gate 1"] == u.or[j]) & (analysis[, "dat2"] == 1))
  analysis2 <- (analysis[ind1, ])
  for (i in 1:(sum(ind1) / length(v1))){
    ind2 <- ((i - 1) * length(v1) + 1):(i * length(v1))
    lines(analysis2[ind2, "EFS gate 2"], analysis2[ind2, "P(wrong stop under H1)"], type = "b", col = i + 1, pch = 19, lwd = 2)
  }
  
  legend("bottomleft", legend = unique(analysis2[, "OR gate 2"]), col = 2:3, pch = 19, lwd = 2, lty = 1, bty = "n", title = "OR gate corresponding to EFS HR:")
  
  if (u.or[j] == 2.5){points(1, analysis2[5, "P(wrong stop under H1)"], pch = 1, cex = 3, col = "blue", lwd = 4)}
  
}

title("Wrong stopping probabilities for varying gating strategies", outer = TRUE, line = -1.3)

dev.off()
graphics.off()






analysis[, c("scenario", "mut.rate", "factor.medOS.medEFS.H0", "HR.OS", "HR.EFS", "Corr.EFSOS", "Npts", "Nevs", "N.interims", "dat1", "dat2", "OR.CR", "EFS gate 1", "OR gate 1", 
             "EFS gate 2", "OR gate 2", "power (no Fut)", "power (consider Fut)", "P(correct stop under H0)", "P(wrong stop under H1)")]






#

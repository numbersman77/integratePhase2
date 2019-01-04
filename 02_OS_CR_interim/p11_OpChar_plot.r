#--------------------------------------------------------------------------------------------------------------------------------------
# analyze results from simulations
#--------------------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------
# plot stopping probabilities
# -------------------------------------------------------
par(mar = c(8, 4, 4, 4), las = 1)
plot(0, 0, type = "n", xlim = c(xmin, xmax), ylim = c(0, 1), xlab = "interim gate (CR odds ratio)", 
     ylab = "", main = "", xaxt = "n", yaxt = "n", bty = "n")
mtext(paste("False Positive = P(continue @ interim | odds ratio = 1.0)\nFalse Negative = P(stop @ interim | odds ratio = ", disp(xstar, 1), ")", sep = ""), line = 6, side = 1)

axis(1, at = seq(xmin, xmax, by = 0.1), labels = seq(xmin, xmax, by = 0.1), line = 0.5)
axis(3, at = seq(xmin, xmax, by = 0.5), labels = seq(xmin, xmax, by = 0.5), line = 0.5)
axis(2, at = seq(0, 1, by = 0.05), labels = seq(0, 1, by = 0.05), col.axis = "blue", line = 0.5)
axis(4, at = seq(0, 1, by = 0.05), labels = seq(0, 1, by = 0.05), col.axis = "red", line = 0.5)

# false positive: P(continue at interim | OR = 1)
lines(or.gates, fp1, col = "blue", type = "l", lwd = 3)

# false negative: P(stop at interim | OR = 2.0)
lines(or.gates, fn1, col = "red", type = "l", lwd = 3)

legend(0.5, 1, c("False Positive", "False Negative"), lty = 1, lwd = 3, 
       text.col = c("blue", "red"), col = c("blue", "red"), bty = "n")

# compute fn and fp for specific gate
segments(xstar, 0, xstar, 1, col = grey(0.5), lty = 2, lwd = 2)
segments(xmin, fpstar, xstar, fpstar, col = "blue", lty = 1, lwd = 2)
segments(xstar, fnstar, xmax, fnstar, col = "red", lty = 1, lwd = 2)


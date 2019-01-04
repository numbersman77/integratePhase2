file <- paste(path, "simulations/02_OS_CR_interim/results/gates", sep = "")
jpeg(paste(file, ".jpg", sep = ""), width = 900, height = 900, quality = 2000, units = "px", pointsize = 20, bg = "white")

par(las = 1, pty = "s")
plot(0, 0, xlim = c(0, 2.5), ylim = c(0, 3.5), type = "n", bty = "n", xaxs = "i", yaxs = "i", xlab = "EFS hazard ratio", ylab = "CR odds ratio")

x1 <- c(0,   10,  10, 0)
y1 <- c(2.5, 2.5, 10, 10)
polygon(x1, y1, density = 5, angle = 45, col = "red", lwd = 2)

x2 <- c(0, 1, 1,  0)
y2 <- c(2, 2, 10, 10)
polygon(x2, y2, density = 5, angle = 315, col = "blue", lwd = 2)

dev.off()
graphics.off()



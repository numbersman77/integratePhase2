# tab <- data.frame(tme, cens, trt, cr.status)
# write.csv(tab, file = paste(path.results, "CR_nonCR_example.csv"), row.names = FALSE)

tab <- read.csv(file = paste(path.results, "CR_nonCR_example.csv"))

tme <- tab[, "tme"]
cens <- tab[, "cens"]
trt <- tab[, "trt"]
cr.status <- tab[, "cr.status"]


file <- paste(path.results, "OS_CR_nonCR", sep = "")
jpeg(paste(file, ".jpg", sep = ""), width = 1500, height = 900, quality = 2000, units = "px", pointsize = 15, bg = "white")

par(mfrow = c(1, 3), mar = c(4.5, 4, 3, 1), oma = c(0, 0, 3, 0), las = 1, cex = 1)

# plot OS for all patients
surv.OS <- Surv(tme, cens)
plot(survfit(surv.OS ~ trt), col = 2:3, xlab = "overall survival", xlim = c(0, 30), xaxs = "i", yaxs = "i")
cph1 <- coxph(surv.OS ~ trt)
hr1 <- exp(cph1$coefficients) 
title(paste("All patients: hazard ratio = ", round(hr1, 3), sep = ""))

# plot OS for CR patients
plot(survfit(surv.OS[cr.status == 0] ~ trt[cr.status == 0]), col = 2:3, xlab = "overall survival", xlim = c(0, 30), xaxs = "i", yaxs = "i")
cph2 <- coxph(surv.OS[cr.status == 0] ~ trt[cr.status == 0])
hr2 <- exp(cph2$coefficients) 
title(paste("Patients with CR: hazard ratio = ", round(hr2, 3), sep = ""))

# plot OS for non-CR patients
plot(survfit(surv.OS[cr.status == 1] ~ trt[cr.status == 1]), col = 2:3, xlab = "overall survival", xlim = c(0, 30), xaxs = "i", yaxs = "i")
cph3 <- coxph(surv.OS[cr.status == 1] ~ trt[cr.status == 1])
hr3 <- exp(cph3$coefficients) 
title(paste("Patients with non-CR: hazard ratio = ", round(hr3, 3), sep = ""))

title("MDM2(4): hazard ratio for OS, for all patients and per CR status @ interim", outer = TRUE)

c(hr1, hr2, hr3)

dev.off()
graphics.off()
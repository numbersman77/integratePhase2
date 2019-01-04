## =================================================================
## generate all plots also as .jpgs
## =================================================================

pdf <- FALSE

file <- paste(path, "02_OS_CR_interim/results/match_S", sep = "")
if (pdf == FALSE){jpeg(paste(file, ".jpg", sep = ""), width = 987, height = 700, quality = 2000, units = "px", pointsize = 20, bg = "white")}
if (pdf == TRUE){pdf(paste(file, ".pdf", sep = ""), width = 7 * sqrt(2), height = 7, pointsize = 10, onefile = TRUE, family = "Helvetica")}
source(paste(path, "02_OS_CR_interim/p02_verify_detailed_survival_functions.r", sep = ""))
dev.off()
graphics.off()

file <- paste(path, "02_OS_CR_interim/results/opchar", sep = "")
if (pdf == FALSE){jpeg(paste(file, ".jpg", sep = ""), width = 987, height = 700, quality = 2000, units = "px", pointsize = 20, bg = "white")}
if (pdf == TRUE){pdf(paste(file, ".pdf", sep = ""), width = 7 * sqrt(2), height = 7, pointsize = 10, onefile = TRUE, family = "Helvetica")}
source(paste(path, "02_OS_CR_interim/p11_OpChar_plot.r", sep = ""))
dev.off()
graphics.off()


# ================================================================= 
# initialize
# =================================================================
packs <- packs <- c("survival", "muhaz", "reporttools")
for (i in 1:length(packs)){library(packs[i], character.only = TRUE)}     

path <- ""

# 01_samplesize_OS
source(paste(path, "functions/qWeibullCure.r", sep = ""))
source(paste(path, "functions/generateTimeToEventData1arm.r", sep = ""))
source(paste(path, "functions/generateTimeToEventData2arm.r", sep = ""))
source(paste(path, "functions/samplesize2logrank.r", sep = ""))
source(paste(path, "functions/mddHR.r", sep = ""))
source(paste(path, "functions/landmarkTest.r", sep = ""))

source(paste(path, "MIRROS_parameters.r", sep = ""))

# 02_OS_CR_interim
source(paste(path, "functions/generateData.r", sep = ""), echo = FALSE)
source(paste(path, "functions/generateStudy_OS.r", sep = ""), echo = FALSE)
source(paste(path, "functions/analyse_OS.r", sep = ""), echo = FALSE)
source(paste(path, "functions/simulate_OS.r", sep = ""), echo = FALSE)
source(paste(path, "functions/correctProbCR.r", sep = ""), echo = FALSE)
source(paste(path, "functions/confIntKM_t0.r", sep = ""), echo = FALSE)
source(paste(path, "functions/quantileKM.r", sep = ""), echo = FALSE)
source(paste(path, "functions/power2logrank.r", sep = ""), echo = FALSE)

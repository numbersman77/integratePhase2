# ---------------------------------------------------------------------------------
# compute estimated survival from KM at given timepoints
# ---------------------------------------------------------------------------------
lm.control <- confIntKM_t0(time = time.os.control, event = rep(1, length(time.os.control)), t0 = seq(0, 60, by = 12), conf.level = 0.95)
lm.tmt <- confIntKM_t0(time = time.os.tmt, event = rep(1, length(time.os.tmt)), t0 = seq(0, 60, by = 12), conf.level = 0.95)


lm1 <- cbind(lm.control[, 1:2], lm.tmt[, 2], lm.tmt[, 2] - lm.control[, 2], 1 / (lm.tmt[, 2] - lm.control[, 2]))
colnames(lm1)[-1] <- c("control", "treatment", "improvement", "NNT") 
lm1[, 2:4] <- round(lm1[, 2:4], 3)
lm1[, 5] <- round(lm1[, 5], 1)


lm1

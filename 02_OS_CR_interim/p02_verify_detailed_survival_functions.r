# verify detailed survival function in terms of input parameters
xs <- seq(0, 100, by = 0.01)

# sample size model
S1  <- cure[1] + (1 - cure[1]) * (1 - pweibull(xs, shape = shape[1], scale = scale[1]))
S2 <- cure[2] + (1 - cure[2]) * (1 - pweibull(xs, shape = shape[2], scale = scale[2]))

# interim endpoint model: control arm
S_n1 <- 1 - pexp(xs, rate = log(2) / med.nonresp.H0)
S_s1 <- 1 - pexp(xs, rate = log(2) / med.resp.shortterm.H0)
S_detail1 <- p1.cr * (p.long.term + (1 - p.long.term) * S_s1) + (1 - p1.cr) * S_n1

par(las = 1)
plot(xs, S1, type = "l", col = "grey", lwd = 5, xlab = "time", ylab = "OS", main = "compare survival functions from sample size and CR - OS model")
lines(xs, S_detail1, col = "red", lwd = 1)

# interim endpoint model: treatment arm
S_n2 <- 1 - pexp(xs, rate = log(2) / (med.nonresp.H0 / HR.OS))
S_s2 <- 1 - pexp(xs, rate = log(2) / (med.resp.shortterm.H0 / HR.OS))
S_detail2 <- p2.cr * (p.long.term + (1 - p.long.term) * S_s2) + (1 - p2.cr) * S_n2

lines(xs, S2, type = "l", col = "grey", lwd = 5)
lines(xs, S_detail2, col = "red", lwd = 1)

legend("topright", c("sample size model", "CR - OS model"), col = c("grey", 2), lwd = c(5, 1), bty = "n")
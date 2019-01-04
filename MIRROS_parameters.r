# MIRROS parameters

## global parameters
alpha <- 0.05
beta <- 0.15
pwt <- 0.85   # probability of wild type

# proportion of patients randomized to control
rando.control <- 1 / 3
r <- (1 - rando.control) / rando.control

## define Weibull distributions
shape1 <- 1
scale1 <- 7.6519
shape2 <- 1
scale2 <- 9.925873

## define cure proportions

# proportion with CR
p1.cr <- 0.16
or <- 2.5
odds1 <- p1.cr / (1 - p1.cr)
p2.cr <- odds1 * or / (1 + odds1 * or)

# probability to be a long-term survivor if you have CR
p.long.term <- 0.5
cure1 <- p.long.term * p1.cr
cure2 <- p.long.term * p2.cr

shape <- c(shape1, shape2)
scale <- c(scale1, scale2)
cure <- c(cure1, cure2)

# define recruitment
# possible to use "non-integer" patients, e.g. if you want to model subgroup with known prevalence
# from overall pop, as in e.g. MIRROS
pts <- c(rep(12, 15), rep(17, 15), 5) 

pts.month <- rbinom(n = length(pts), size = pts, prob = pwt)
pts.control <- rbinom(n = length(pts), size = pts.month, prob = rando.control)
pts.tmt <- pts.month - pts.control
recruit <- list(pts.control, pts.tmt)

n1 <- sum(pts.control)
n2 <- sum(pts.tmt)
Npt <- n1 + n2

## number of events to compute power for
d <- 275

# drop-out (5% annually)
delta1 <- 0.05
tau1 <- -log(1 - delta1) / 12
delta2 <- 0.05
tau2 <- -log(1 - delta2) / 12

## number of runs for simulation
nsim <- 10 ^ 5

## medians of mixtures
m1 <- qWeibullCure(0.5, p0 = cure1, shape = shape1, scale = scale1)
m2 <- qWeibullCure(0.5, p0 = cure2, shape = shape2, scale = scale2)
med <- c(m1, m2)

## medians of exponentials with these rates
m1star <- qWeibullCure(0.5, p0 = 0, shape = shape1, scale = scale1)   # same as log(2) * scale1
m2star <- qWeibullCure(0.5, p0 = 0, shape = shape2, scale = scale2)   # same as log(2) * scale2
medstar <- c(m1star, m2star)

## mdd under Exponentiality
mdd <- mddHR(d = d, alpha = alpha, kappa = 1 / (r + 1))

## necessary number of events under Exponentiality
d_exp <- samplesize2logrank(alpha = alpha, beta = beta, hr = m1 / m2, kappa = 1 / (r + 1))




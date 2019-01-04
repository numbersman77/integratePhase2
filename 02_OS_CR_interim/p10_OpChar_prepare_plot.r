#--------------------------------------------------------------------------------------------------------------------------------------
# analyze results from simulations
# analysis2 generated in p09
#--------------------------------------------------------------------------------------------------------------------------------------

# analysis2
load(file = paste(path, "02_OS_CR_interim/results/analysis2", sep = ""))

# -------------------------------------------------------
# define grid of interim gates, i.e. x-axis of plot
# -------------------------------------------------------
xmin <- 0.5
xmax <- 4
gates <- seq(xmin, xmax, by = 0.02)

# false positive: P(continue at interim | OR = 1)
os1 <- with(as.data.frame(analysis2), is.na(`OS gate`))
fp1 <- 1 - subset(as.data.frame(analysis2), 
                 subset = (scenario == 1 & os1), 
                 select = "P(correct stop under H0)")[, 1]

# false negative: P(stop at interim | OR = 2.0)
fn1 <- subset(as.data.frame(analysis2), 
             subset = (scenario == 2 & os1),
             select = "P(wrong stop under H1)")[, 1]

# compute fn and fp for specific gate
fpstar <- fp1[or.gates == xstar]
fnstar <- fn1[or.gates == xstar]

# power loss taking into account interim
powerloss <- subset(as.data.frame(analysis2), 
                    subset = (scenario == 2 & os1 & or.gates == xstar),
                    select = "power (consider Fut)")[, 1]



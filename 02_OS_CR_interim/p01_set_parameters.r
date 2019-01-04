#-------------------------------------------------------------------------------------------------------------------------------------
# Generate OS and EFS data under H0 (with CR rate of 10%)
#-------------------------------------------------------------------------------------------------------------------------------------

# number of event times to simulate for the basic time-to-event datasets
# from these we will draw study event times
n <- 500000

# median 1: median OS for non-responders in ARA-C arm
med.nonresp.H0 <- 5.13

# median 2: median OS for responders but short-time survivors in ARA-C arm
med.resp.shortterm.H0 <- 7.5

# longterm survival
long.surv <- 240

# assumed effect:
OR.CR <- 2.5
HR.OS <- 0.8

# recruitment
recruit <- pts

# mutation rate
mut.rate <- 0




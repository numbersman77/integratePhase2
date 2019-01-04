generateTimeToEventData1arm <- function(shape = 1, scale, cure = 0, recruit, dropout = 0, start.accrual = 0, cutoff, seed = NA){
  
  # shape             Weibull shape parameter. 
  # scale:            Weibull scale parameter
  # cure:             Proportion of patients assumed to be cured, i.e. with an event at +infty.
  # recruit:          Recruitment.
  # dropout:          Drop-out rate, on same time scale as med.
  # start.accrual:    Time unit where accrual should start. Might be useful when simulating multi-stage trials.
  # cutoff:           Cutoff, #events the final censored data should have (can be a vector of multiple cutoffs).
  # seed:             If different from NA, seed used to generate random numbers.
  #
  # Kaspar Rufibach, June 2014
  
  if (is.na(seed) == FALSE){set.seed(seed)}
  
  n <- sum(recruit)
  
  # generate arrival times
  arrive <- rep(1:length(recruit), times = recruit)
  arrivetime <- NULL
  for (i in 1:n){arrivetime[i] <- runif(1, min = arrive[i] - 1, max = arrive[i])}
  arrivetime <- start.accrual + sort(arrivetime)
  
  # generate event times: Exp(lambda) = Weibull(shape = 1, scale = 1 / lambda)
  eventtime <- qWeibullCure(runif(n), p0 = cure, shape = shape, scale = scale)
  
  # Apply drop-out. Do this before applying the cutoff below, in order to correctly count necessary #events.
  dropouttime <- rep(Inf, n)
  if (dropout > 0){dropouttime <- rexp(n, rate = dropout)}
  event.dropout <- ifelse(eventtime > dropouttime, 0, 1)
  time.dropout <- ifelse(event.dropout == 1, eventtime, dropouttime)   
  
  # observed times, taking into account staggered entry
  tottime <- arrivetime + eventtime
  
  # find cutoff based on number of targeted events
  # only look among patients that are not considered dropped-out
  time <- data.frame(matrix(NA, ncol = length(cutoff), nrow = n))
  event <- time
  cutoff.time <- rep(NA, length(cutoff))
  
  for (j in 1:length(cutoff)){
    cutoff.time[j] <- sort(tottime[event.dropout == 1])[cutoff[j]]
    
    # apply administrative censoring at cutoff
    event[event.dropout == 1, j] <- ifelse(tottime[event.dropout == 1] > cutoff.time[j], 0, 1)
    event[event.dropout == 0, j] <- 0
    
    # define time to event, taking into account both types of censoring
    time[event.dropout == 1, j] <- ifelse(event[, j] == 1, eventtime, cutoff.time[j] - arrive)[event.dropout == 1]    # same as: pmin(tottime, cutoff.time) - arrive
    time[event.dropout == 0, j] <- pmin(cutoff.time[j] - arrivetime, time.dropout)[event.dropout == 0]
    
    # remove times for patients arriving after the cutoff
    rem <- (arrivetime > cutoff.time[j])
    if (TRUE %in% rem){time[rem, j] <- NA}
  }
  
  # generate output
  tab <- data.frame(cbind(1:n, arrivetime, eventtime, tottime, dropouttime, time, event))
  colnames(tab) <- c("pat", "arrivetime", "eventtime", "tottime", "dropouttime", paste("time cutoff = ", cutoff, sep = ""), paste("event cutoff = ", cutoff, sep = ""))
  
  res <- list("cutoff.time" = cutoff.time, "tab" = tab)
  return(res)
}
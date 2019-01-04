correctProbCR <- function(OR.CR = 2.5, p0 = prob.CR, mut.rate = mut.rate){

  # CR rate in wild-type patients such that overall rate in MDM2(4) arm provides fixed odds.ratio 
  # (0.1 * p0 + 0.9 * p1.corr = p1), e.g. mut rate = 0, then p1.corr = p1 
  odds.p0 <- p0 / (1 - p0)

  # p1 = CR rate in tmt group, assuming odds in control group and OR
  p1 <- (OR.CR * odds.p0) / (1 + OR.CR * odds.p0)
  p1.corr <- (p1 - mut.rate * p0) / (1 - mut.rate) 

  return(p1.corr)
}
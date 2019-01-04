# proportion of cured patients
prob.CR <- c(0.16, correctProbCR(OR.CR = 2.5, p0 = 0.16, mut.rate = 0))
prob.longterm <- c(0.5, 0.5)

# before interim
n1 <- c(40, 80) * 0.85
n1 * prob.CR
n1 * prob.CR * prob.longterm

# after interim
n2 <- c(1/3, 2/3) * (440 - sum(n1)) * 0.85
n2 * prob.CR
n2 * prob.CR * prob.longterm

# total over trial
n <- n1 + n2
n * prob.CR
n * prob.CR * prob.longterm

sum(ceiling(n1 * prob.CR * prob.longterm) + ceiling(n2 * prob.CR * prob.longterm)) / sum(n)











#
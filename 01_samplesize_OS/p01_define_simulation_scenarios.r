#--------------------------------------------------------------------------------------------------------------------------------------
# Define scenarios for simulations   
#--------------------------------------------------------------------------------------------------------------------------------------

scenarios <- rbind(
  c(1, shape1, shape1, scale1, scale1, cure1, cure1, tau1, tau2, d, nsim, 1111),
  c(2, shape1, shape2, scale1, scale2, cure1, cure2, tau1, tau2, d, nsim, 2222),
  
  c(3, shape1, shape1, m1,     m1,     0,     0,     tau1, tau2, d_exp, nsim, 3333),
  c(4, shape1, shape2, m1,     m2,     0,     0,     tau1, tau2, d_exp, nsim, 4444),
  
  c(5, shape1, shape1, scale1, scale1, cure1, cure1, tau1, tau2, d_exp, nsim, 5555),
  c(6, shape1, shape2, scale1, scale2, cure1, cure2, tau1, tau2, d_exp, nsim, 6666)
)

colnames(scenarios) <- c(
  "scenario", "shape.control", "shape.tmt", "scale.control", "scale.tmt", 
  "cure.control", "cure.tmt", "dropout.control", "dropout.tmt", "d", "nsim", "seed")
scenarios

# milestone survival times at which to evaluate power
t0s <- c(12, 18, 24, 30, 36)




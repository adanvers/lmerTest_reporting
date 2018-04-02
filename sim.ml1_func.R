# Generate Fake Multi-Level Data

# J is number of clusters, K is number of measures per cluster
sim.ml1 <- function(J=J, K=K, b0=1, b1=0.5, vsub.b0=0.2, vsub.b1=0.2, vresid=0.2) {
  lvl2 <- rep(seq(1,K,length=K)) # K represents measures per cluster
  id <- rep(1:J, each=K) # J represents number of clusters
  b0.int <- rnorm(J, b0, sqrt(vsub.b0)) # generating random intercept for each cluster
  b1.int <- rnorm(J, b1, sqrt(vsub.b1)) # effect of continuous predictor
  x <- runif(J*K, 1, 7) # values of continuous predictor
  # generating the outcome, based on the specified model
  y <- rnorm(J*K, b0.int[id] + b1.int[id]*x, sqrt(vresid))
  sim.dat <- as.data.frame(cbind(id,x,y))
  return(sim.dat)
} 

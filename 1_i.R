# Input parameters
set.seed(666)
mu = 1
sigmasq = 1
nSims = 10000

for(N in c(100,1000,10000)){
  # Create nSims datasets
  ## Store in matrix X (N X nSims )®
  ## Each column corresponds to one simulated dataset
  X = matrix ( NA , nrow = N , ncol = nSims ) # Initialize
  
  # Generate the samples 
  X = matrix ( rnorm ( n = N*nSims , mean = mu , sd = sqrt ( sigmasq )) ,
               nrow = N , ncol = nSims )
  
  # Calculate estimators and there properties
  sampleMeans = colMeans(X, na.rm = FALSE, dims = 1)
  varEstimator = var(sampleMeans)
  muEstimator = mean(sampleMeans)
  
  # Calculate pivot quantity 
  P = matrix ( NA , nrow = 1 , ncol = nSims) # Initialize
  for ( s in 1: nSims ) {
    # Draw from univariate normal distribution .
    ## Input is standard deviation instead of variance .
    ## Fill the s^th column
    P [1,s] = (sqrt(N)*(sampleMeans[s] - mu))/sqrt(sigmasq)
  }
  
  cat("For N =", N,"\n")
  # plot estimator data
  seq_norm = seq(min(-10) ,max(10) , length =1000)
  hist (sampleMeans , freq = FALSE , breaks = 20)
  norm_curve <- dnorm (seq_norm , mean = mean (muEstimator) ,sd= sqrt (varEstimator))
  lines(seq_norm , norm_curve , col=" blue ", lwd =2)
  cat("The estimated value of the sample set is: ", muEstimator, "\n")
  cat("The variance of the sample set is: ", varEstimator, "\n")
  
  # plot pivot quantity
  seq_norm = seq(min(-10) ,max(10) , length =1000)
  hist (P , freq = FALSE , breaks = 20)
  norm_curve <- dnorm (seq_norm , mean = mean(P) ,sd= sd(P))
  lines(seq_norm , norm_curve , col=" blue ", lwd =2)
  cat("The estimated value of the pivot quantity set is: ", mean(P), "\n")
  #used sd^2 because there is an undefined error when using var
  cat("The variance of the pivot quantity set is: ", sd(P)^2, "\n")
}

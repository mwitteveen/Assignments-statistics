# Input parameters
set.seed(666)
mu = 0
sigmasq = 1
N = 20
nSims = 10000
# Create nSims datasets
## Store in matrix X (N X nSims )®
## Each column corresponds to one simulated dataset
X = matrix ( NA , nrow = N , ncol = nSims ) # Initialize

# Generate the samples 
X = matrix ( rnorm ( n = N*nSims , mean = mu , sd = sqrt ( sigmasq )) ,
             nrow = N , ncol = nSims )
# Calculate estimators and there properties
sampleMeans = colMeans(X, na.rm = FALSE, dims = 1)

# Calculate pivot quantity 
z = matrix ( NA , nrow = 1 , ncol = nSims) # Initialize
for ( s in 1: nSims ) {
  # Draw from univariate normal distribution .
  ## Input is standard deviation instead of variance .
  ## Fill the s^th column
  z [1,s] = (sampleMeans[s])/(sqrt(sigmasq)/sqrt(N))
}

# plot estimator data
seq_norm = seq(min(-10) ,max(10) , length =1000)
hist (z , freq = FALSE , breaks = 20)
norm_curve <- dnorm (seq_norm , mean = mean(z) ,sd=sd(z))
lines(seq_norm , norm_curve , col=" blue ", lwd =2)
cat("The estimated value of the sample set is: ", mean(z), "\n")
cat("The variance of the sample set is: ", sd(z)^2, "\n")
for (alpha in c(0.01,0.05,0.10)){
  cat("For alpha = ", alpha, " we reject H0 if mu < ", qnorm(alpha, mean = mean(z), sd = sd(z), lower.tail = TRUE, log.p = FALSE), "\n")
}

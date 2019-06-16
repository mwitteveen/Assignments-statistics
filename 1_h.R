# Input parameters
set.seed(666)
mu = 1
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
varEstimator = var(sampleMeans)
muEstimator = mean(sampleMeans)

# plot estimator data
seq_norm = seq(min(-10) ,max(10) , length =1000)
hist (sampleMeans , freq = FALSE , breaks = 20)
norm_curve <- dnorm (seq_norm , mean = mean(muEstimator) ,sd= sqrt (varEstimator))
lines(seq_norm , norm_curve , col=" blue ", lwd =2)
cat("The estimated value of the sample set is: ", muEstimator, "\n")
cat("The variance of the sample set is: ", varEstimator)


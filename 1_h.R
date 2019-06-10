# Input parameters
mu = 1
sigmasq = 1
N = 20
nSims = 10000
# Create nSims datasets
## Store in matrix X (N X nSims )®
## Each column corresponds to one simulated dataset
# Option 1: with loop
X = matrix ( NA , nrow = N , ncol = nSims ) # Initialize
for ( s in 1: nSims ) {
  # Draw from univariate normal distribution .
  ## Input is standard deviation instead of variance .
  ## Fill the s^th column
  X [ , s ] = rnorm ( n = N , mean = mu , sd = sqrt ( sigmasq ))
}
# Option 2: without loop ( faster )
X = matrix ( rnorm ( n = N*nSims , mean = mu , sd = sqrt ( sigmasq )) ,
             nrow = N , ncol = nSims )
# Plot histogram of x with normal curve

seq_norm = seq(min(-10) ,max(10) , length =1000)
hist (x , freq = FALSE , breaks = 20)
norm_curve <- dnorm (seq_norm , mean = mean (mu) ,sd= sqrt (sigmasq))
lines(seq_norm , norm_curve , col=" blue ", lwd =2)

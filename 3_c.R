# Input parameters
set.seed(666)
mu = 0
sigmasq = 0.5
N = 10
nSims = 100000
beta1 = 1
beta2 = -1

# Create nSims datasets
## Store in matrix X (N X nSims )®
## Each column corresponds to one simulated dataset
Y = matrix ( NA , nrow = N , ncol = nSims ) # Initialize
x1 = rnorm(N, mean = 0, sd = 1)
x2 = rbinom(N, size=1, prob=.5)
xMatrix = rbind(x1,x2)
tX = t(xMatrix)
estimateBeta = tX%*%xMatrix

for ( j in 1: nSims ) {
  for ( i in 1:N){
    Y [i,j] = rnorm ( n = 1 , mean = (x1[i]*beta1 + x2[i]*beta2) , sd = sqrt ( sigmasq )) 
  }
}

# Option 2: without loop ( f
# plot estimator data
seq_norm = seq(min(-10) ,max(10) , length =1000)
hist (Y , freq = FALSE , breaks = 20)
norm_curve <- dnorm (seq_norm , mean = mean(Y) ,sd=sd(Y))
lines(seq_norm , norm_curve , col=" blue ", lwd =2)
cat("The estimated value of the sample set is: ", mean(Y))
cat("The variance of the sample set is: ", sd(Y)^2)



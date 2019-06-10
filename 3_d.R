# Input parameters
set.seed(666)
mu = 0
sigmasq = 0.5

nSims = 100000
beta1 = 1
beta2 = -1

for (N in c(100,1000)){
  # Create nSims datasets
  ## Store in matrix X (N X nSims )®
  ## Each column corresponds to one simulated dataset
  Y = matrix ( NA , nrow = N , ncol = nSims ) # Initialize
  x1 = rnorm(N, mean = 0, sd = 1)
  x2 = rbinom(N, size=1, prob=.5)
  
  for ( j in 1: nSims ) {
    for ( i in 1:N){
      Y [i,j] = rnorm ( n = 1 , mean = (x1[i]*beta1 + x2[i]*beta2) , sd = sqrt ( sigmasq )) 
    }
  }
  
  #This part of the calculation for Beta estimate doesn't change
  xMatrix = rbind(x1,x2)
  
  library(MASS)
  #calculate all estimators for beta 
  estimateBeta = ginv(xMatrix%*%t(xMatrix))%*%(xMatrix%*%Y)
  
  # plot estimator data
  hist (estimateBeta , freq = FALSE , breaks = 20)
  
  #print details
  cat("N=", paste(N), "\n")
  cat("The estimated value of beta 1 is: ", mean(estimateBeta[1,]), "\n")
  cat("The variance of beta 1 is: ", sd(estimateBeta[1,])^2, "\n")
  cat("The estimated value of beta 2 is: ", mean(estimateBeta[2,]), "\n")
  cat("The variance of beta 2 is: ", sd(estimateBeta[2,])^2, "\n")
}
cat("Finished")

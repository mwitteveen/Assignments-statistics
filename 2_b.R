# Input parameters
set.seed(666)
mu = -1
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

# for each alpha
for (alpha in c(0.01, 0.05, 0.10)){
  
  counter = 0
  for ( s in 1: nSims ){
    upperbound = sampleMeans[s]+qnorm(1-(alpha/2))*sqrt(sigmasq/N)
    lowerbound = sampleMeans[s]-qnorm(1-(alpha/2))*sqrt(sigmasq/N)
    if ((mu <= upperbound)&(mu >= lowerbound)){
      counter = counter + 1
    }
  }
  cat("For alpha is ", alpha, " the true value of mu lies for ", counter/nSims, "% in the confidence interval \n")
}
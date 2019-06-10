# Input parameters
set.seed(666)
nSims = 10000

for(N in c(20, 200, 2000)){
  # Create nSims datasets
  ## Store in matrix X (N X nSims )®
  ## Each column corresponds to one simulated dataset
  Y = matrix ( NA , nrow = N , ncol = nSims )
  pbar = matrix ( NA , nrow = 3 , ncol = nSims )
  for ( s in 1: nSims ) {
    draws = rmultinom ( n = N , size = 1 , prob = c(0.1,0.3,0.6) )
    for ( i in 1: N ) {
      Y [i , s ] = which ( draws [ , i ] == 1)
    }
    for ( i in 1:3 ) {
      pbar [i , s ] = length(which(Y[,s] == i))/N
      pbar[is.na(pbar)] <- 0
    }
  }
  
  for(i in 1:3){
    # plot estimator data
    hist (pbar[i,] , freq = FALSE , breaks = 20, main=paste("Histogram P",paste(i), " and N=", paste(N)))
    
    #print details
    cat("The estimated value of p", i, "is: ", mean(pbar[i,]), "\n")
    cat("The variance of p", i, "is: ", sd(pbar[i,])^2, "\n")
  }
}

# Input parameters
set.seed(666)
nSims = 100000
N = 20
p = c(0.1,0.3,0.6)
gctorture(FALSE)

#generate data sets and estimators
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
  if ((s %% 1000) == 0){ cat(s, "\n")}
}
# for each alpha
for (alpha in c(0.01, 0.05, 0.10)){
    # for each p
    for (j in 1:3){
      counter = 0
      for ( s in 1: nSims ){
        upperbound = pbar[j,s]+qnorm(1-(alpha/2))*sqrt((pbar[j,s]*(1-pbar[j,s]))/N)
        lowerbound = pbar[j,s]-qnorm(1-(alpha/2))*sqrt((pbar[j,s]*(1-pbar[j,s]))/N)
        if ((p[j] <= upperbound)&(p[j] >= lowerbound)){
          counter = counter + 1
        }
      }
      cat("For alpha is ", alpha, " the percentage of estimator p", j, " that is in the confidence interval is ", counter/nSims, "\n")
  }
}


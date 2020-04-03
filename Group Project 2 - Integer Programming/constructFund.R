constructFund = function(rho, q, priceMat, sharesMat, unique_tickers, unique_dates){
  library(lpSolve)
  
  #objective coefficients
  
  c = c(rep(0,100),(as.vector(rho)))
  
  
  #LHS of constraints
  
  A<-matrix(0,10101,10100)
  
  
  # first constraint 1 row
  # second constraint 100 rows - one for each value of j
  # third constraint 100 x 100 rows - for each combination of i & j
  
  #constraint 1
  
  A[1,1:100] = c(rep(1,100))
  
  
  #constraint 2
  
  for (i in 1:100){
    A[i+1, ((100*i)+1):((i+1)*100)] = rep(1,100)
  }
  
  #constraint 3
  
  A[102:10101,101:10100] = diag(1,10000,10000)
  
  for (i in 1:100){
    A[(102+((i-1)*100)):((102+((i-1)*100))+99),1:100] = diag(-1,100,100)
  }
  
  #RHS of constraints
  b<-c(q,rep(1,100),rep(0,10000))
  
  #Direction vector
  dir<-c(rep("=",101),rep("<=",10000))
  
  ## solving the ip
  library(lpSolve)
  lp1=lp("max",c,A,dir,b,all.bin = TRUE)
  
  
  sol = lp1$solution
  
  xij = matrix(sol[101:10100],100,100,byrow = TRUE)
  y = sol[1:100]
  
  #getting market cap at last day of 2012
  
  market_cap = priceMat["2012-12-31", ]*sharesMat["2012-12-31",]
  
  weights = (market_cap %*% xij)
  
  weight_prop = as.vector(weights/sum(weights))
#  names(weight_prop) <- as.character(unique_tickers)
  
  
  return(weight_prop)
  #return(lp1$objval)
  #return(sol)
}
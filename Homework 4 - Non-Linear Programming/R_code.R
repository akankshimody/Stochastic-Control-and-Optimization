## Question 1
func1 <- function(k){ 
  machines= 0.05 * (((100000-(15*k))/12)^(2/3)) * (k^(1/3))
  return(-machines)
}
S=optim(100,func1,method="CG")
S$par #127.5942
S$value #-102.2061

##Question 2
library(matrixStats)
library(quadprog)
stocks = read.csv("homework4stocks.csv", header = TRUE)
stocks
tempdf = data.matrix(stocks[c(2:ncol(stocks))])
tempdf
colMeans(tempdf)
colVars(tempdf)
cor(data.matrix(stocks[c(2:ncol(stocks))]), method = 'pearson')
ncol(stocks)

m=colMeans(tempdf)
s=colSds(tempdf)

rho=cor(data.matrix(stocks[c(2:ncol(stocks))]), method = 'pearson')

covMat=diag(s) %*% rho %*% diag(s)

RVal=0.01
Dmat=2*covMat
dvec=rep(0,ncol(stocks)-1)
Amat=matrix(c(rep(1,27),rep(-1,27),m,-m),27)
bvec=c(1,-1,0.01,-0.01)
  
#no shorting
Amat=cbind(Amat,diag(27))
bvec=c(bvec,rep(0,27))
  
S=solve.QP(Dmat,dvec,Amat,bvec)
S$solution
S$value
new_portfolio = S$solution
mean(new_portfolio)
var(new_portfolio)
sd(new_portfolio)


#Question 3
var_selection = read.csv("variable_selection.csv", header = TRUE, sep =",")
model1 = lm(y~x1, data = var_selection)
summary(model1)
model2 = lm(y~x2, data = var_selection)
summary(model2)
model3 = lm(y~x3, data = var_selection)
summary(model3)
model4 = lm(y~x1+x2, data = var_selection)
summary(model4)
model5 = lm(y~x1+x3, data = var_selection)
summary(model5)
model6 = lm(y~x2+x3, data = var_selection)
summary(model3)

#Question4

library(quadprog)

Dmat=2*matrix(c(1,0,0,0,0,
                0,4,0,0,0,
                0,0,6,0,0,
                0,0,0,12,0,
                0,0,0,0,3),5,5)
dvec=rep(0,5)
Amat=matrix(c(1,1,0,0,0,
              -1,-1,0,0,0,
              0,0,0,1,1,
              0,0,0,-1,-1,
              1,0,-1,-1,0,
              -1,0,1,1,0,
              0,1,1,0,-1,
              0,-1,-1,0,1),5,8)
bvec=c(710,-710,710,-710,0,0,0,0)


S=solve.QP(Dmat,dvec,Amat,bvec)
S$solution
S$value

#Question5
nfl = read.csv("nflratings.csv", header = FALSE, sep = ',')
nfl$ActualPointSpread = nfl$V4-nfl$V5
func5 <- function(ratings){
  #ratings = c(1:33)
  #min_rating = min(ratings[1:32])
  #max_rating = max(ratings[1:32])
  std_rating = sd(ratings)
  avg_rating = 85
  normalized_ratings = c(rep(0,33))
  for (i in 1:32){
    normalized_ratings[i] = (ratings[i]-avg_rating)/std_rating
  }
  normalized_ratings[33] = ratings[33] 
  nfl$predicted_spread = normalized_ratings[nfl$V2]-normalized_ratings[nfl$V3] + normalized_ratings[33]
  prediction_error = sum((nfl$ActualPointSpread-nfl$predicted_spread)^2)
  return (prediction_error)
}

random_start = c(rep(85,32),3)
S=optim(random_start,func5,method="CG")

#Question 5 diff approach
nfl = read.csv("nflratings.csv", header = FALSE, sep = ',')
nfl$ActualPointSpread = nfl$V4-nfl$V5
func5 <- function(ratings){
  nfl$predicted_spread = ratings[nfl$V2]-ratings[nfl$V3] + ratings[33]
  prediction_error = sum((nfl$ActualPointSpread-nfl$predicted_spread)^2)
  return (prediction_error)
}

random_start = c(rep(85,32),3)
S=optim(random_start,func5,method="CG")

rm(list = ls())

#setwd("D:/SPRING SEMESTER/Stochastic Control and Optimization/Projects/Project2")

data = read.csv("N100StkPrices.csv", header = TRUE)

# clean up data
data = na.omit(data)
ticker = data$TICKER

# spun off MDLZ
delete = seq(1, dim(data)[1])[ticker == "MDLZ"]
data = data[-delete, ]

date = apply(as.matrix(data$date), MARGIN = 1, FUN = "toString")
date = as.Date(date, "%Y%m%d")
ticker = data$TICKER
price = data$PRC
shares = data$SHROUT


# Accounting for changes in ticker names



# KFT changed to KRFT in Oct 2012.
ticker[ticker == "KFT"] = "KRFT"

# SXCI changed to CTRX in Jul 2012.
ticker[ticker == "SXCI"] = "CTRX"

# HANS changed to MNST in Jan 2012.
ticker[ticker == "HANS"] = "MNST"




# convert prices to a matrix, arranged by rows of dates and columns of tickers
unique_dates = sort(unique((date)))
unique_tickers = sort(unique(ticker))

priceMat = matrix(NA, length(unique_dates), length(unique_tickers))
sharesMat = matrix(0, length(unique_dates), length(unique_tickers))


for (i in 1:length(unique_tickers)) {
  tic = unique_tickers[i]
  idx = is.element(unique_dates, date[ticker == tic])
  
  priceMat[idx, i] = price[ticker == tic]
  sharesMat[idx, i] = shares[ticker == tic]
}

rownames(priceMat) = as.character(unique_dates)
rownames(sharesMat) = as.character(unique_dates)

rm(list = c("data", "delete", "i", "idx", "price", "shares", "tic", "ticker", "date"))



# Read Monthly Data -------------------------------------------------------

# read in the data
mdata = read.csv("N100Monthly.csv", header = TRUE, stringsAsFactors = FALSE)

# clean up data
mdate = apply(as.matrix(mdata$date), MARGIN = 1, FUN = "toString")
mdate = as.Date(mdate, "%Y%m%d")

mticker = mdata$TICKER
mprice = mdata$PRC
mshares = mdata$SHROUT
mticker[mticker == "FOXA"] = "NWSA"


unique_mdates = sort(unique((mdate)))
unique_mtickers = sort(unique(mticker))

idx = is.element(unique_mtickers, unique_tickers)

# if (!all(idx)) {
#   print("Warning: Some tickers seem to be missing")
# }

monthlyPriceMat = matrix(NA, length(unique_mdates), length(unique_tickers))

for (i in 1:length(unique_tickers)) {
  tic = unique_tickers[i]
  idx = is.element(unique_mdates, mdate[mticker == tic])
  monthlyPriceMat[idx, i] = mprice[mticker == tic]
}

rm("mdata", "i", "idx", "mprice", "mshares", "mticker", "tic", "mdate")




#########QUESTION1


growthMat = matrix(NA, length(unique_dates)-1, length(unique_tickers))


for (i in 1: nrow(growthMat)) {
  for (j in 1: ncol(growthMat)){
    growthMat[i,j] = (priceMat[i+1,j] - priceMat [i,j])/priceMat[i,j]
  }
}

rownames(growthMat) = as.character(unique_dates[2:250])


##########QUESTION2

rho = cor(growthMat,use = "pairwise.complete.obs")

rownames(rho) = as.character(unique_tickers)



############ QUESTION3



source('constructFund.R')
#Weights for each stock

q = 25
weights = constructFund(rho, q, priceMat, sharesMat, unique_tickers, unique_dates)


#Check1 - sum of weights add up to 1
sum(weights)

#Check2 - number of stocks selected is 25
sum(weights > 0)


weights




############ QUESTION4

#Number of shares at the end of 2012

investment = 1000000
dec_end_portfolio = data.frame("Ticker" = as.character(unique_tickers),"Weights" = as.vector(weights))

dec_end_portfolio$investment = as.vector(dec_end_portfolio$Weights * investment)

price_dec_end = priceMat["2012-12-31",]
price_dec_beg = priceMat["2012-12-03",]

dec_end_portfolio$price_dec_end = as.vector(price_dec_end)
dec_end_portfolio$price_dec_beg = as.vector(price_dec_beg)
dec_end_portfolio$sharesbought = as.vector(dec_end_portfolio$investment / dec_end_portfolio$price_dec_end)

#dec_end_portfolio = dec_end_portfolio[dec_end_portfolio$Weights > 0,]

## Value at beginning of Dec 2012

dec_beg_value = sum(dec_end_portfolio$sharesbought * dec_end_portfolio$price_dec_beg)
cat('Value at beginning of december is',dec_beg_value)


## Index stock in Dec 2012

index_dec_2012 = 2660.93
index_stock = investment/index_dec_2012
cat('Amount of Index stock purchased in Dec 2012 ',index_stock)


## fund growth for 12 months

fund_growth = matrix(0,12,2)

initial_value = investment

for (i in 1:12){
  final_value = sum(monthlyPriceMat[i,] * dec_end_portfolio$sharesbought)
  growth = (final_value - initial_value)/initial_value
  fund_growth[i,1] = i
  fund_growth[i,2] = growth
#  initial_value = final_value
}


## index growth for 12 months

nasdaq_growth = matrix(0,12,2)
nasdaq_values = c(2731.53, 2738.58, 2818.69, 2887.44,
                 2981.76, 2909.60, 3090.19, 3073.81, 3218.20, 3377.73, 3487.82, 3592.00)

initial_value = index_dec_2012

for (i in 1:12){
  final_value = nasdaq_values[i]
  growth = (final_value - initial_value)/initial_value
  nasdaq_growth[i,1] = i
  nasdaq_growth[i,2] = growth
#  initial_value <- final_value
}


plot(nasdaq_growth[,1],nasdaq_growth[,2],type="l",col="red", ylab = 'Returns', xlab = 'Month')
lines(nasdaq_growth[,1],fund_growth[,2],col="green")
legend(8,0.12,legend=c("NASDAQ","Portfolio"),col=c("red","green"),lty=c(1,2))


############ QUESTION5

source('similarityMat.R')
source('constructFund.R')

q = 25

rho = similarityMat(priceMat, sharesMat, unique_tickers,unique_dates);
weights = constructFund(rho, q, priceMat, sharesMat, unique_tickers, unique_dates)


investment = 1000000
dec_end_portfolio = data.frame("Ticker" = as.character(unique_tickers),"Weights" = as.vector(weights))

dec_end_portfolio$investment = as.vector(dec_end_portfolio$Weights * investment)

price_dec_end = priceMat["2012-12-31",]
price_dec_beg = priceMat["2012-12-03",]

dec_end_portfolio$price_dec_end = as.vector(price_dec_end)
dec_end_portfolio$price_dec_beg = as.vector(price_dec_beg)
dec_end_portfolio$sharesbought = as.vector(dec_end_portfolio$investment / dec_end_portfolio$price_dec_end)

#dec_end_portfolio = dec_end_portfolio[dec_end_portfolio$Weights > 0,]

## Value at beginning of Dec 2012

dec_beg_value = sum(dec_end_portfolio$sharesbought * dec_end_portfolio$price_dec_beg)
cat('Value at beginning of december is',dec_beg_value)


## Index stock in Dec 2012

index_dec_2012 = 2660.93
index_stock = investment/index_dec_2012
cat('Amount of Index stock purchased in Dec 2012 ',index_stock)


## fund growth for 12 months

fund_growth = matrix(0,12,2)

initial_value = investment

for (i in 1:12){
  final_value = sum(monthlyPriceMat[i,] * dec_end_portfolio$sharesbought)
  growth = (final_value - initial_value)/initial_value
  fund_growth[i,1] = i
  fund_growth[i,2] = growth
  #  initial_value = final_value
}


## index growth for 12 months

nasdaq_growth = matrix(0,12,2)
nasdaq_values = c(2731.53, 2738.58, 2818.69, 2887.44,
                  2981.76, 2909.60, 3090.19, 3073.81, 3218.20, 3377.73, 3487.82, 3592.00)

initial_value = index_dec_2012

for (i in 1:12){
  final_value = nasdaq_values[i]
  growth = (final_value - initial_value)/initial_value
  nasdaq_growth[i,1] = i
  nasdaq_growth[i,2] = growth
  #  initial_value <- final_value
}


plot(nasdaq_growth[,1],nasdaq_growth[,2],type="l",col="red", ylab = 'Returns', xlab = 'Month')
lines(nasdaq_growth[,1],fund_growth[,2],col="green")
legend(8,0.12,legend=c("NASDAQ","Portfolio"),col=c("red","green"),lty=c(1,2))



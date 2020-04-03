# You need to set your own directory

# read in the data
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



#---Question1----

daily_returns = matrix(NA,nrow(priceMat),ncol(priceMat))
rownames(daily_returns) = unique_dates
colnames(daily_returns) = unique_tickers

for (i in 2:nrow(priceMat)){
  for (j in 1:ncol(priceMat)){
    daily_returns[i,j] = (priceMat[i,j]-priceMat[i-1,j])/priceMat[i-1,j]
  }
}
daily_returns

#----Question2---

similarity_mat = cor(daily_returns, use="pairwise.complete.obs")


#---Question3----

library(lpSolve)
constructFund <- function(rho, q, priceMat, sharesMat, unique_tickers, unique_dates){
  c = c(as.vector(rho),rep(0,n))
  n = length(unique_tickers)
  b = c(q, rep(1,n),rep(0,n^2))
  A = matrix(0, n^2+n+1, n^2+n)
  A[1,(n^2+1):(n^2+n)] = rep(1,n)
  for (i in 1:n){
    A[(i+1), (n*(i-1)+1):(n*i)] = rep(1,n)
  }
  A[(n+2):(n^2+n+1), 1:n^2] = diag(1,n^2)
  A[(n+2):(n^2+n+1), (n^2+1):(n^2+n)] = matrix(rep(diag(-1,n),n), nrow=n^2, byrow=T)
  dir = c(rep('=',(n+1)),rep('<=',n^2))
  s <- lp('max', c, A, dir, b, all.bin=TRUE)
  


#Market Cap is number of shares* last price point
market_cap = sharesMat[length(unique_dates),]*priceMat[length(unique_tickers),]
for (i in 1:n){
}


}

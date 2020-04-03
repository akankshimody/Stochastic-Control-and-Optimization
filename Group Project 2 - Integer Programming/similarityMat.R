similarityMat = function(priceMat, sharesMat, unique_tickers,unique_dates){
  library(lsa)
  growthMat = matrix(NA, length(unique_dates)-1, length(unique_tickers))
  
  
  for (i in 1: nrow(growthMat)) {
    for (j in 1: ncol(growthMat)){
      growthMat[i,j] = (priceMat[i+1,j] - priceMat [i,j])/priceMat[i,j]
    }
  }
  

  
  rownames(growthMat) = as.character(unique_dates[2:250])
  newmat = cosine(growthMat)
  newmat[is.na(newmat)] <- 0
  return (newmat)
}


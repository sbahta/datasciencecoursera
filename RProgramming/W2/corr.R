corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  corVector <- numeric()
  
  FileNames <- list.files(directory,pattern="*.csv",full.names=TRUE)

  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  for(oneFile in FileNames)
  {
    tmpData <- read.csv(oneFile)
    
    tmpComplete <- complete.cases(tmpData)
    
    tmpCompleteData <- tmpData[tmpComplete,c("sulfate","nitrate")]
    
    if(nrow(tmpCompleteData) > threshold)
    {
       var1 <- cor(tmpCompleteData,use="all.obs")
       corVector <- c(corVector,var1[1,2])
    }
  }
  
  if(length(corVector) == 0 )
    corVector <- c(0)
  
  corVector
}
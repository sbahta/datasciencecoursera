complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  index <- 0L  
  nobs = numeric(length=length(id))
    
  for(i in id){
      fileName <- paste(directory,"/",formatC(i,width=3,flag="0"),".csv",sep='')
      index <- index+1L
    
      ## read from fileName to dataframe.   
      MyData <- read.csv(fileName)
    
      ## check for non NA values
      CompleteCaseRecords <- complete.cases(MyData)
    
      ## get the non NA records
      CleanData <- MyData[CompleteCaseRecords,]
    
      ##count non NA records. 
      nobs[index] <- nrow(CleanData)  
  }
  
  myDataFrame <- data.frame(id,nobs)  
  print (myDataFrame)  
  
  }
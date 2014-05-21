pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vvector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

  pollutantMeans <- numeric()
  index<-0
  
  for(i in id){
    fileName <- paste(directory,"/",formatC(i,width=3,flag="0"),".csv",sep='')
    tmpData <- read.csv(fileName)
    pollutantMeans <- c(pollutantMeans,tmpData[,pollutant]) 
    ##index <- index + 1  
    ##pollutantMeans[index]= mean(tmpData[,pollutant],na.rm=TRUE)  
  }
  
  ##pollutantMeans
  print(length(pollutantMeans))
  boolValues <- complete.cases(pollutantMeans)
  ActualValues <- pollutantMeans[boolValues]
  mean(ActualValues)
  
  ## mean(pollutantMeans,na.rm=TRUE)
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
}
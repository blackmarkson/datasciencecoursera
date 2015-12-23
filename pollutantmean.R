pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  # get a list of files inthe directory  
  directory <- paste(directory, "/", sep="")
  files <- list.files(directory)
  for (file in files) { # iterate through file list
    path <- paste(directory, file, sep="")
    fileid <- as.numeric(substr(file,1,3))
    if (fileid %in% id) { # if file in input parameters
      # incorporate data from this file 
      if (!exists("pollutants")){
        pollutants <- read.csv(path)
      }
      else {
       # if pollutants exists then append to it
        pollutants<-rbind(pollutants, read.csv(path))  
      }
    }
  }
  mean(pollutants[[pollutant]],na.rm=TRUE) #return mean
}

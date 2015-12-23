corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  # get a list of files inthe directory
  directory <- paste(directory, "/", sep="")
  files <- list.files(directory)
  correllations <- numeric() # the empty correlation vector
  for (file in files) {
    path <- paste(directory, file, sep="")
    pollutants <- read.csv(path) # read the next file in the list
    pollutants <- subset(pollutants,!is.na(sulfate) & !is.na(nitrate)) # ignore NAs
    if (nrow(pollutants) > threshold) { # threshold met
      print(nrow(pollutants))
      # append correlation for this file
      correllations <- append(correllations, cor(pollutants[,2], pollutants[,3])) 
    }
  }
  correllations
}
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
  # get a list of files inthe directory
  directory <- paste(directory, "/", sep="")
  files <- list.files(directory)
# create empty return vector  
  nobservations <- data.frame(id = integer(0), nobs = integer(0))
  for (testID in id) {
    for (file in files) {
      path <- paste(directory, file, sep="")
      fileid <- as.numeric(substr(file,1,3))
      if (fileid %in% testID) {
#         add number of full observations to the return vector        
          pollutants <- read.csv(path)
          pollutants <- subset(pollutants,!is.na(sulfate) & !is.na(nitrate))
          nobservations = rbind(nobservations, c(fileid, nrow(pollutants)))
      }
    }
  }
  colnames(nobservations) <- c("id", "nobs")
  nobservations
}
corr <- function(directory, threshold = 0) {
        
        # Return a data frame of the form where ID is the monitoring id and 'nobs' is the number of complete cases
        #files_list <- list.files(directory, full.names = TRUE) # creates a list of files
        #dat <- data.frame() # create an empty data frame
        # Return a data frame of the form where ID is the monitoring id and 'nobs' is the number of complete cases
        
        correlationVector = NULL ## initializing the correlation matrix
        
        for (i in 1:332) {
                
                #data <- read.csv(
                #paste(as.character(i), ".csv", sep=""),     ## Normal
                #header = T, 
                #na.strings=c("NA","NaN", " ") 
                
                files_list <- list.files(directory, full.names = TRUE)
                dat <- data.frame() # create an empty data frame
                # Loops through the files, rbinding them tohether
                
                dat <- rbind(dat, read.csv(files_list[i], na.strings=c("NA", "NaN", " ")))                      
                
                ## getting rid of all the "NA" values and, consequently, all the non-complete ovservations (the ones with at least one NA in row)
                data = na.omit(dat) 
                
                ## if the number of complete observed cases meets the quota, find the correlation between the pollutants for the given monitor AND
                ## store the results in the correlation matrix
                if (nrow(data) > threshold) {
                        correlationVector = c(correlationVector, cor(data[,2], data[,3]))
                }
                
                
        }
        
        return (correlationVector)
}
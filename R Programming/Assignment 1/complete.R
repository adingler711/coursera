complete <- function(directory, id = 1:332){
        # Return a data frame of the form where ID is the monitoring id and 'nobs' is the number of complete cases
        #files_list <- list.files(directory, full.names = TRUE) # creates a list of files
        #dat <- data.frame() # create an empty data frame
        id_len <- length(id)
        complete_data <- rep(0, id_len) 
        
        j <- 1
        
        for (i in id){
                files_list <- list.files(directory, full.names = TRUE)
                dat <- data.frame() # create an empty data frame
                # Loops through the files, rbinding them tohether
                dat <- rbind(dat, read.csv(files_list[i]))
                
                complete_data[j] <- sum(complete.cases(dat))
                j <- j +1  
        }
        result <- data.frame(id = id, nobs = complete_data)
        return(result)
}
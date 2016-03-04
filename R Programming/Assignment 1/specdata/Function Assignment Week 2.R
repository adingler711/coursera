
# Part 1 of assignment 1 ####
pollutantmean <- function(directory, pollutant, id =1:332){
        #return the mean of the pollutant across all monitors list in the "id" vector (ignoring NA values)
        files_list <- list.files(directory, full.names = TRUE) # creates a list of files
        dat <- data.frame() # create an empty data frame
        for (i in id){
                # Loops through the files, rbinding them tohether
                dat <- rbind(dat, read.csv(files_list[i]))
        }
        # dat_subset <- dat[which(dat[,"Day"] == day), ] # subsets the roows that match the "day" argument
        mean(dat[, pollutant], na.rm = TRUE) # identifies the median weight
        # while stripping out the NAs
}

pollutantmean("specdata", "sulfate", 1:10) # 4.064128 # Correct
pollutantmean("specdata", "nitrate", 70:72) # 1.706047 # Correct
pollutantmean("specdata", "nitrate", 23) # 1.280833 # Correct


# Part 2 of assignment 1 ####

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
complete("specdata", 1) # 1 1 117 # Correct
complete("specdata", c(2, 4, 8, 10, 12)) #nobs is currently providing a running sum; moved files_list and empty data frame inside the for loop to resart the nob count for each file
complete("specdata", 30:25) 
complete("specdata", 3) # 1 3 243 # Correct


# Part 3 of assignment 1 ####

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

cr <- corr("specdata", 150)
head(cr) # -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
         # -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
summary(cr)

cr <- corr("specdata", 400)
head(cr)

cr <- corr("specdata")
summary(cr)

length(cr)

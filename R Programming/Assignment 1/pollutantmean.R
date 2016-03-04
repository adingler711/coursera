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
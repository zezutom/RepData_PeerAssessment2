###
# Reproducible Research: Peer Assessment 2
###

### Configuration

# Show the code
echo = TRUE

# Change working directory to the script location
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)


### Dependencies

## Verifies that the required libraries get installed if needed
libs <- function(...) {
  lapply(list(...), function(lib) {
    if (!lib %in% installed.packages()) {
      install.packages(lib)
    }
    library(lib, character.only = TRUE)
  })
}
libs("R.utils", "data.table")


### Load data: 
storm_data_file <- "StormData.csv"
if (!file.exists(storm_data_file)) {
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  dest_file <- "StormData.csv.bz2"
  download.file(url, destfile = dest_file, method = "curl")
  bunzip2(dest_file, overwrite = TRUE, remove = TRUE)
}

storm_data <- fread(storm_data_file, header = TRUE, sep = ",", verbose = TRUE)




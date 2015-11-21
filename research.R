###
# Reproducible Research: Peer Assessment 2
###

### Configuration

# Show the code
echo = TRUE
options(scipen = 1)  # Turn off scientific notations for numbers

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
libs("R.utils", "data.table", "dplyr")

### Data (Down)load
storm_data_file <- "StormData.csv"
if (!file.exists(storm_data_file)) {
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  dest_file <- "StormData.csv.bz2"
  download.file(url, destfile = dest_file, method = "curl")
  bunzip2(dest_file, overwrite = TRUE, remove = TRUE)
}

storm_data <- fread(storm_data_file, header = TRUE, sep = ",", verbose = TRUE)

### Data Analysis

## Preparation

# Add a new column representing a year. That will help with understanding trends.
storm_data$YEAR <- as.numeric(
  format(as.Date(storm_data$BGN_DATE, "%m/%d/%Y %H:%M:%S"), "%Y"))

# Reduce columns to only those needed for answering the questions
health_data <- c("FATALITIES", "INJURIES")          # Health
economic_data <- c("PROPDMG", "CROPDMG")            # Economy

storm_data <- subset(storm_data, select=c(
  "YEAR",                         # Timeline
  "EVTYPE",                       # Weather Events 
  health_data,
  economic_data,
  "PROPDMGEXP", "CROPDMGEXP"      # Economy (extent)
))

## PROPDMGEXP and CROPDMGEXP represent the scale of property and crop damage.
## They are effectively multipliers of the values in PROPDMG and CROPDMG columns.

# First of all, standardize multiplier values
storm_data <- mutate_each(storm_data, funs(tolower), 
                          which(colnames(storm_data) %in% economic_data_mult))

# Next, multiply the cost of damage
multiplier <- function(m, x) {
  m_val <- switch(m, 
         "h" = 2,
         "k" = 3,
         "m" = 6,
         "b" = 9,
         0)
  return (x * 10^m_val)
}
storm_data <- mutate(storm_data, PROPDMG = mapply(multiplier, storm_data$PROPDMGEXP, storm_data$PROPDMG))
storm_data <- mutate(storm_data, CROPDMG = mapply(multiplier, storm_data$CROPDMGEXP, storm_data$CROPDMG))

# Plot a histogram to find out about representative years
png("plot1.png")
hist(storm_data$YEAR, 
     breaks = 20, 
     main = "Extreme Weather Events per Year",
     xlab = "Year")
dev.off()

# Most extreme weather events happened between 1990 - 2010
storm_data <- subset(storm_data, (YEAR >= 1990) & (YEAR <= 2010))

# Summarize weather events by the given health or economic aspect. 
# Results are plotted as a histogram.
sum_by_aspect <- function(x) {
  # Summarize weather events by the given health aspect
  summary <- aggregate(subset(storm_data, select = x), list(storm_data$EVTYPE), "sum")
  
  # Sort in a descending order (most frequent first)
  setorderv(summary, x, -1)
  
  print(head(summary))
  
  # TODO render a histogram
}

## Q1. Across the United States, which types of events 
## (as indicated in the EVTYPE variable) are most harmful 
## with respect to population health?
lapply(health_data, sum_by_aspect)

## Q2. Across the United States, which types of events have 
## the greatest economic consequences?
lapply(economic_data, sum_by_aspect)

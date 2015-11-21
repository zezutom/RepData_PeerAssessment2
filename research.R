###
# Reproducible Research: Peer Assessment 2
###

### Configuration

# Show the code
echo = TRUE

# Change working directory to the script location
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

### Utility Functions

# Quantifies multiplication symbols as an nth power of ten.
multiply <- function(m, x) {
  m_val <- switch(m, 
                  "h" = 2,
                  "k" = 3,
                  "m" = 6,
                  "b" = 9,
                  0)
  return (x * 10^m_val)
}

# Draws a bar chart.
# df        .. data frame containing the plotted data
# weight    .. columnar weight
# ylab      .. y-axis label
# ylab_unit .. y-axis unit (optional)
# title     .. a distinct part of a chart title
bar_chart <- function(df, weight, ylab, ylab_unit = NA, title, filename) {
  png(paste(filename, "png", sep = "."))
  ylab_title <- paste(ylab, title)
  if (!is.na(ylab_unit)) {
    ylab_title <- paste(ylab_title, "in", ylab_unit)
  }
  print (qplot(EVTYPE, data = fatalities, weight = weight, geom = "bar", binwidth = 1) + 
           scale_y_continuous(ylab_title) + 
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
           xlab("Extreme Weather Event") + 
           ggtitle(paste(title, 
                         "by Extreme Weather Events in the US\nbetween", 
                         year_from, "and", year_to)))
  dev.off()
}

# Transforms a column name into a description
as_title <- function(x) {
  # Capitalize
  title <- strsplit(x, " ")[[1]]
  title <- paste(toupper(substring(title, 1,1)), tolower(substring(title, 2)), sep = "")
  
  # Handle special parts
  gsub("dmg$", " Damage", title)
}

# Summarizes weather events by the given health or economic aspect. 
# Results are plotted as a histogram.
sum_by_aspect <- function(x, ylab, ylab_unit = NA, cap = 10, debug = FALSE) {
  # Summarize weather events by the given health aspect
  summary <- aggregate(subset(storm_data, select = x), list(EVTYPE = storm_data$EVTYPE), "sum")
  
  # Sort in a descending order (most frequent first)
  setorderv(summary, x, -1)
  
  # Consider Top N (Top 10 by default) most severe events
  top_n <- head(summary, cap)
  
  if (debug) {
    print(top_n)  
  }
  
  # Plot it on a bar chart
  bar_chart(top_n, top_n[, x], ylab, ylab_unit, as_title(x), tolower(x))
}

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

## Import all libraries
libs("R.utils", "data.table", "dplyr", "ggplot2", "grid", "gridExtra")

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

# Add a new column representing a year. That will help with understanding trends.
storm_data$YEAR <- as.numeric(
  format(as.Date(storm_data$BGN_DATE, "%m/%d/%Y %H:%M:%S"), "%Y"))


# Reduce columns to only those needed for answering the questions
health_data <- c("FATALITIES", "INJURIES")          # Health
economic_data <- c("PROPDMG", "CROPDMG")            # Economy
economic_data_mult <- c("PROPDMGEXP", "CROPDMGEXP") # Economy (extent)

storm_data <- subset(storm_data, select=c(
  "YEAR",                         # Timeline
  "EVTYPE",                       # Weather Events 
  health_data,
  economic_data,
  economic_data_mult      
))

## PROPDMGEXP and CROPDMGEXP represent the scale of property and crop damage.
## They are effectively multipliers of the values in PROPDMG and CROPDMG columns.

# First of all, standardize multiplier values
storm_data <- mutate_each(storm_data, funs(tolower), 
                          which(colnames(storm_data) %in% economic_data_mult))

# Next, multiply the cost of damage
storm_data <- mutate(storm_data, PROPDMG = mapply(multiply, storm_data$PROPDMGEXP, storm_data$PROPDMG))
storm_data <- mutate(storm_data, CROPDMG = mapply(multiply, storm_data$CROPDMGEXP, storm_data$CROPDMG))

# Plot a histogram to find out about representative years
png("extreme_years.png")
hist(storm_data$YEAR, 
     breaks = 20, 
     main = "Extreme Weather Events per Year",
     xlab = "Year")
dev.off()

# Most extreme weather events happened between 1990 - 2010
year_from <- 1990
year_to <- 2010
storm_data <- subset(storm_data, (YEAR >= year_from) & (YEAR <= year_to))


## Q1. Across the United States, which types of events 
## (as indicated in the EVTYPE variable) are most harmful 
## with respect to population health?
lapply(health_data, function(x) {
  sum_by_aspect(x, "Number of")
})

## Q2. Across the United States, which types of events have 
## the greatest economic consequences?
lapply(economic_data, function(x) {
  sum_by_aspect(x, "Cost of", "US Dollars")
})

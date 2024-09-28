# file: "loadBuoy.R"
# author: "Taha Ababou"

# a) Your first exercise is to read in the data for all the years from 1985 to 2023. As discussed in class, you don’t want to do this 
# manually and will need to figure out a way to do it programmatically. We’ve given you a skeleton of how to do this for data for one 
# year below. Your task is to adapt this to reading in multiple datasets from all the years in question. This example code is meant to 
# be a guide and if you think of a better way to read the data in, go for it.

# Keep in mind that initially, these datasets did not record units and then started to do so in the line below the column headers. So for 
# some years you will have to skip 1 instead of 2.

################################################################################################################################################

library(data.table)
library(lubridate)

file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
tail <- ".txt.gz&dir=data/historical/stdmet/"
all_years_data <- list()
max_columns <- c("YY", "MM", "DD", "hh", "mm", "WD", "WSPD", "GST", "WVHT", 
                 "DPD", "APD", "MWD", "BAR", "ATMP", "WTMP", "DEWP", "VIS", "TIDE", "Year")

read_buoy_data <- function(year) {
  path <- paste0(file_root, year, tail)
  header <- scan(path, what = 'character', nlines = 1, quiet = TRUE)
  skip_lines <- ifelse(year < 2007, 1, 2)
  buoy_data <- fread(path, header = FALSE, skip = skip_lines, fill = TRUE)
  setnames(buoy_data, header[1:min(length(header), ncol(buoy_data))])
  missing_cols <- setdiff(max_columns, colnames(buoy_data))
  for (col in missing_cols) {
    buoy_data[, (col) := NA_real_]
  }
  setcolorder(buoy_data, max_columns)
  buoy_data[, Year := as.integer(year)]  # Ensure 'Year' is assigned as an integer
  return(buoy_data)
}

for (year in 1985:2023) {
  buoy_data <- tryCatch(read_buoy_data(year),
                        error = function(e) {
                          message(paste("Failed to load data for year:", year))
                          return(NULL)
                        })
  if (!is.null(buoy_data)) {
    all_years_data[[as.character(year)]] <- buoy_data
  }
}

combined_data <- rbindlist(all_years_data, fill = TRUE)
combined_data[, YY := as.integer(YY)]  # Ensure 'YY' is treated as an integer
combined_data[, Date := ymd_hm(paste(YY, MM, DD, hh, mm, sep = "-"))]
combined_data[, c("YY", "#YY", "YYYY") := NULL]

# (Part D debugging) -- Creating a valid datetime column for combined_data
combined_data <- combined_data %>%
  mutate(
    # Replace missing hour, month, or day with reasonable defaults
    hh = ifelse(is.na(hh), 0, hh),            # Replace missing hour with 0 (midnight)
    MM = ifelse(is.na(MM), 1, MM),            # Replace missing month with January
    DD = ifelse(is.na(DD), 1, DD),            # Replace missing day with the 1st
    # Create the datetime column from Year, MM, DD, and hh
    datetime = make_datetime(year = Year, month = MM, day = DD, hour = hh)
  )


write.csv(combined_data, "buoy_44013_1985_2023.csv", row.names = FALSE)
print(head(combined_data))

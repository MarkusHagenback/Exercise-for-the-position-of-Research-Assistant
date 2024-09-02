#########################################
##   Reading in the data from Github   ##
#########################################
# This makes it possible to only need this R-script to read the entire code
# Thus, no need to copy the data-files etc.
library(readxl) # Library needed to read in the file correctly

# URL of the raw file
url <- "https://raw.githubusercontent.com/MarkusHagenback/Exercise-for-the-position-of-Research-Assistant/main/exercise.xlsx"
temp_file <- tempfile(fileext = ".xlsx") #Create a temporary file to store the downloaded Excel file

# Download the Excel file to the temporary location
download.file(url, destfile = temp_file, mode = "wb")

# Read the Excel file into R
data <- read_excel(temp_file)

# Delete the temporary file after use
rm(temp_file)
rm(url)

#########################################
##   Data cleaning   ##
#########################################
# Making date column as.date
data$date <- as.Date(data$date, format = "%Y-%m-%d")  # Adjust format if needed

# Rating column has some non-numeric values. Will remove these ones, as they do not contain any information for us, however, need to remember which one in order to improve the data collection for the future (to find what was the problem with this data collection)

# Identify rows where 'average rating' is non-numeric
troublesome_rows <- which(!grepl("^\\d+(\\.\\d+)?$", data$`average rating`))

# View the actual rows
print(data[troublesome_rows, ])
# Removing these rows
data <- data[-troublesome_rows, ]

# Converting to numeric
data$`average rating` <- as.numeric(data$`average rating`)

#########################################
##   Reading in the data from Github   ##
#########################################
rm(list=ls())
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

#######################
##   Data cleaning   ##
#######################
# Making date column as.date
data$date <- as.Date(data$date, format = "%Y-%m-%d")  # Adjust format if needed

# Rating column has some non-numeric values. Will remove these ones, as they do not contain any information for us, however, need to remember which one in order to improve the data collection for the future (to find what was the problem with this data collection)

# Identify rows where 'average rating' is non-numeric
troublesome_rows_non_numeric <- which(!grepl("^\\d+(\\.\\d+)?$", data$`average rating`))

# Converting to numeric
data$`average rating` <- as.numeric(data$`average rating`)

# We also have some NA question(s), which needs to be removed, as these contains little information
# Identify rows where 'question' is NA
troublesome_rows_question <- which(is.na(data$question))

# Then we have question topic which is NA. But based on the information from other rows, we know what it should be
# Identify rows where 'question topic' is NA
na_question_topic <- which(is.na(data$`question topic`))

# For each row with NA in 'question topic', find the corresponding value from other rows with the same 'question'
for (i in na_question_topic) {
  # Find the 'question' value for the current row with NA
  question_value <- data$question[i]

  # Find the most common 'question topic' for rows with the same 'question'
  # Filter rows where 'question' matches and exclude NA 'question topic'
  matching_rows <- data[data$question == question_value & !is.na(data$`question topic`), ]

  if (nrow(matching_rows) > 0) {
    # Use the most frequent 'question topic' from matching rows
    most_common_topic <- names(sort(table(matching_rows$`question topic`), decreasing = TRUE))[1]
    data$`question topic`[i] <- most_common_topic
  }
}

# Combine all troublesome rows and remove duplicates
troublesome_rows_all <- unique(c(troublesome_rows_non_numeric, troublesome_rows_question, na_question_topic))

##
# Create a lookup table for the most common 'question topic' for each 'question'
most_common_topics <- aggregate(`question topic` ~ question, data = data, function(x) {
  names(sort(table(x), decreasing = TRUE))[1]
})

# Convert to a named vector for easier lookup
common_topic_lookup <- setNames(most_common_topics$`question topic`, most_common_topics$question)

# Vector to store row numbers where updates are made
troublesome_rows_wrong_topic <- integer(0)

# Update rows where 'question topic' is different from the majority
for (i in seq_len(nrow(data))) {
  if (!(i %in% troublesome_rows_all)) {   # Check if the row index is not in the troublesome rows list
    question_value <- data$question[i]
    
    # Determine the correct 'question topic' from the lookup table
    correct_topic <- common_topic_lookup[question_value]
    
    if (data$`question topic`[i] != correct_topic) {
      # Save the row number where the update happens
      troublesome_rows_wrong_topic <- c(troublesome_rows_wrong_topic, i)  
      
      # Update 'question topic' to the most common topic
      data$`question topic`[i] <- correct_topic
    }
  }
}

# Combine all troublesome rows and remove duplicates
troublesome_rows_all <- unique(c(troublesome_rows_all, troublesome_rows_wrong_topic))

# Removing these troublesome rows
data <- data[-troublesome_rows_all, ]

##

# Finally, it is some missing values for the other columns. However, it is still okay to use those rows for analysis. As this rows still containts information that is valuable. For instance, even if we dont know the sponsor organisation, we can still use the row to analyse virtual classes

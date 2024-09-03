rm(list=ls()) # Removes everything from the work environment
#########################################
##   Reading in the data from Github   ##
#########################################
# This approach makes it possible to only need this R-script to read the entire code
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
################################################################################
#   Data cleaning
################################################################################
# Making date column as.date
data$date <- as.Date(data$date, format = "%Y-%m-%d")  # Adjust format if needed

# Convert `course was virtual` to a factor
data <- data %>%
  mutate(`course was virtual` = factor(`course was virtual`, 
                                       levels = c(0, 1),
                                       labels = c("Non-Virtual", "Virtual")))
#############################################
# Rating column has some non-numeric values # 
#############################################
#Will remove these ones (later), as they do not contain any information

# Identify rows where 'average rating' is non-numeric
troublesome_rows_non_numeric <- which(!grepl("^\\d+(\\.\\d+)?$", data$`average rating`)) # Will remove later

# Converting to numeric
data$`average rating` <- as.numeric(data$`average rating`)

################
# NA questions #
################
# Will be removed, as these contains little information

# Identify rows where 'question' is NA
troublesome_rows_question <- which(is.na(data$question))

########################
# Question topic is NA # 
########################
# Based on the information from other rows, we know what it should be

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

########################
# Wrong question topic #
########################

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
  # Skip the row if 'question topic' or 'question' is NA (these are already fixed)
  if (is.na(data$`question topic`[i]) || is.na(data$question[i])) {
    next
  }
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

##############################
# Creating a dataframe/table #
##############################
# For what we have done with the data, for the report

# Create individual dataframes for each list with explanations
df_na_question_topic <- data.frame(
  RowNumber = na_question_topic,
  Explanation = "NA question topic (Updated)"
)

df_wrong_topic <- data.frame(
  RowNumber = troublesome_rows_wrong_topic,
  Explanation = "Wrong question topic (Updated)"
)

df_non_numeric <- data.frame(
  RowNumber = troublesome_rows_non_numeric,
  Explanation = "Non-numeric value in average rating (Removed)"
)

df_na_question <- data.frame(
  RowNumber = troublesome_rows_question,
  Explanation = "NA question (Removed)"
)
  
# Combine all dataframes into one
data_cleaning_table <- rbind(df_na_question_topic, df_wrong_topic, df_non_numeric, df_na_question)

# Some rows needs to be removed, insufficient data to analyse
troublesome_rows_to_remove <- unique(c(troublesome_rows_non_numeric, troublesome_rows_question))

# Removing these troublesome rows
data <- data[-troublesome_rows_to_remove, ]

# Finally, there is some missing values for the other columns. 
# However, it is still okay to use those rows for analysis. 
# As these rows still contains information that is valuable. 
# For instance, even if we don't know the sponsor organisation, 
# we can still use the row to analyse virtual classes

################################################################################
#                     Cleaning up the environment

# Define the objects to keep
objects_to_keep <- c("data", "data_cleaning_table")

# Get a list of all objects in the environment
all_objects <- ls()

# Identify objects to remove (those not in the list of objects to keep)
objects_to_remove <- setdiff(all_objects, objects_to_keep)

# Remove the objects that are not in the keep list
rm(list = objects_to_remove)
rm(all_objects)
rm(objects_to_remove)
################################################################################
#   End of data cleaning
################################################################################

################################################################################
#   Data analysis
################################################################################
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# Average rating by virtual vs. in-person
virtual_vs_inperson <- data %>%
  group_by(`course was virtual`) %>%
  summarise(avg_rating = mean(`average rating`, na.rm = TRUE))

# Average rating by sponsor organization
rating_by_sponsor <- data %>%
  group_by(`sponsoring organisation`) %>%
  summarise(avg_rating = mean(`average rating`, na.rm = TRUE))

# Trends over time
# Aggregate by date: Average rating per date
daily_avg_rating <- data %>%
  group_by(date) %>%
  summarise(avg_rating = mean(`average rating`, na.rm = TRUE))

# Plotting the average rating over time (by day)
ggplot(daily_avg_rating, aes(x = date, y = avg_rating)) +
  geom_line() +
  labs(title = "Average Rating Over Time (by Date)", x = "Date", y = "Average Rating")
# Comment: Low rating at: 2019-03-24 (4.125).

#######################
# Evolution of Question Topics Over Time:

# Would be easier to understand if we would like week-by-week basis

# Aggregating by week and question topic
data$week <- floor_date(data$date, "week")

weekly_topic_rating <- data %>%
  group_by(week, `question topic`) %>%
  summarise(avg_rating = mean(`average rating`, na.rm = TRUE))

# Plotting the evolution of question topics over time
ggplot(weekly_topic_rating, aes(x = week, y = avg_rating, color = `question topic`)) +
  geom_line() +
  labs(title = "Evolution of Question Topics Over Time", x = "Week", y = "Average Rating", color = "Question Topic")
# Comment: Very low value for presentation methods at: 2019-03-24 (3.95)

#################

# Aggregating by week and question
weekly_question_rating <- data %>%
  group_by(week, question) %>%
  summarise(avg_rating = mean(`average rating`, na.rm = TRUE))

# Plotting the evolution of specific questions over time
ggplot(weekly_question_rating, aes(x = week, y = avg_rating, color = question)) +
  geom_line() +
  labs(title = "Evolution of Specific Questions Over Time", x = "Week", y = "Average Rating", color = "Question")
# Comment: Low ratings at: 2019-03-24 (3.60) for "Lecturers encouraged discussion and participation"
# Comment: Low ratings at: 2017-05-21 (3.90) for "The session with the participants' presentations was useful"

###########################################
# Evolution of Virtual Courses Over Time:

# Count of virtual courses per week
virtual_course_count <- data %>%
  filter(!is.na(date), !is.na(`course was virtual`)) %>%
  group_by(week, `course was virtual`) %>%
  summarise(course_count = n())
# Comment: Changed to only virtual from 2018-10-25

# Plotting the count of virtual courses over time
ggplot(virtual_course_count, aes(x = week, y = course_count, fill = `course was virtual`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Virtual vs In-Person Courses Over Time", x = "Week", y = "Course Count", fill = "Virtual")

# Average rating for virtual vs in-person over time
weekly_virtual_rating <- data %>%
  filter(!is.na(date), !is.na(`course was virtual`)) %>%
  group_by(week, `course was virtual`) %>%
  summarise(avg_rating = mean(`average rating`, na.rm = TRUE))

# Plotting the average rating for virtual vs in-person over time
ggplot(weekly_virtual_rating, aes(x = week, y = avg_rating, color = `course was virtual`)) +
  geom_line() +
  labs(title = "Average Rating for Virtual vs In-Person Courses Over Time", x = "Week", y = "Average Rating", color = "Virtual")

# Calculate weekly average rating for each question, split by virtual/non-virtual
weekly_question_rating <- data %>%
  filter(!is.na(date), !is.na(`course was virtual`)) %>%
  group_by(week, question, `course was virtual`) %>%
  summarise(avg_rating = mean(`average rating`, na.rm = TRUE))

# Each unique question over time
ggplot(weekly_question_rating, aes(x = week, y = avg_rating, color = `course was virtual`)) +
  geom_line() +
  facet_wrap(~question) +
  labs(title = "Average Rating for Virtual vs In-Person Courses Over Time",
       x = "Week", y = "Average Rating", color = "Virtual") +
  theme_minimal()

############################
# Table for each question, virtual vs non-virtual
# Calculate the average rating for each question, split by virtual/non-virtual
question_comparison <- data %>%
  filter(!is.na(date), !is.na(`course was virtual`)) %>%
  group_by(question, `course was virtual`) %>%
  summarise(avg_rating = mean(`average rating`, na.rm = TRUE), 
            count = n())  # Include count of responses for each group

# Pivot the data to create a table with separate columns for virtual and non-virtual
question_comparison_table <- question_comparison %>%
  pivot_wider(names_from = `course was virtual`, 
              values_from = c(avg_rating, count))
############################
# Now to look at interaction between organisation and virtual

# Filter out NA values in `course was virtual` and `sponsoring organisation`
interaction_data <- data %>%
  filter(!is.na(`course was virtual`), !is.na(`sponsoring organisation`)) %>%
  group_by(`sponsoring organisation`, `course was virtual`) %>%
  summarise(avg_rating = mean(`average rating`, na.rm = TRUE), 
            count = n())  # Include count of responses for each group

# Plotting the interaction using facet_wrap for each sponsoring organisation
ggplot(interaction_data, aes(x = `course was virtual`, y = avg_rating, fill = `course was virtual`)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ `sponsoring organisation`, scales = "free_x") +
  labs(title = "Average Ratings for Virtual vs. In-Person Courses by Sponsoring Organization",
       x = "Course Type", y = "Average Rating", fill = "Course Type") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        strip.text = element_text(size = 10))  # Adjust facet labels

##############################
# Each sponsorings organisation evultion over time

# Aggregate data by sponsoring organization and date
rating_trends <- data %>%
  filter(!is.na(date), !is.na(`sponsoring organisation`), !is.na(`average rating`)) %>%
  group_by(`sponsoring organisation`, date) %>%
  summarise(avg_rating = mean(`average rating`, na.rm = TRUE)) %>%
  ungroup()

ggplot(rating_trends, aes(x = date, y = avg_rating, color = `sponsoring organisation`)) +
  geom_line(size = 1) +  # Line plot for trends
  geom_point(size = 2) +  # Points on the lines for visibility
  labs(title = "Average Course Ratings Over Time by Sponsoring Organization",
       x = "Date", y = "Average Rating", color = "Sponsoring Organization") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        strip.text = element_text(size = 10))  # Adjust facet labels if needed
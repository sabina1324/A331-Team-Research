setwd("G:/Hertfordshire Class/Team Research & Development/Tanvir/dataset")

getwd()

# Load necessary libraries
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(readr)
library(dplyr)
library(ggplot2)

# Set the directory where your CSV files are located
setwd("G:/Hertfordshire Class/Team Research & Development/Tanvir/dataset")

# List all CSV files in the directory
file_list <- list.files(pattern = "*.csv")

# Initialize an empty list to store the data frames
data_list <- list()

# Function to clean and convert columns to numeric
clean_and_convert <- function(data) {
  # Identify columns with potential type mismatches
  columns_to_convert <- c("price", "engine size2")
  
  # Convert specified columns to numeric
  for (col in columns_to_convert) {
    if (col %in% names(data)) {
      data[[col]] <- as.numeric(gsub("[^0-9.]", "", data[[col]]))
    }
  }
  return(data)
}

# Loop through the list of files and read each one
for (i in seq_along(file_list)) {
  # Construct the full file path
  file_path <- file.path(getwd(), file_list[i])
  
  # Read the CSV file
  data <- read_csv(file_path)
  
  # Clean and convert columns to numeric
  data <- clean_and_convert(data)
  
  # Append the data frame to the list
  data_list[[i]] <- data
}

# Combine all data frames into one using bind_rows
combined_data <- bind_rows(data_list)

# Display the first few rows of the combined data
head(combined_data)

# Summary of the combined data
summary(combined_data)

# Create a scatter plot for example variables (replace variable1 and variable2 with actual variable names)
ggplot(combined_data, aes(x = year, y = price)) +
  geom_point() +
  labs(title = "Scatter Plot of Variable1 vs Variable2",
       x = "year",
       y = "price")

# Save the plot as a PNG file
ggsave("scatter_plot.png")

# Perform a simple linear regression analysis
model <- lm(price ~ year, data = combined_data)

# Display the summary of the regression model
summary(model)

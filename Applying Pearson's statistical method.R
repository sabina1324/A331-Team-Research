# Install necessary packages if not already installed
install.packages("tidyverse")
install.packages("readr")

# Load the necessary libraries
library(tidyverse)

# Set the directory for your datasets
directory <- "G:/Hertfordshire Class/Team Research & Development/archive"

# List all the files in the directory (assuming they are CSV files)
files <- list.files(directory, full.names = TRUE, pattern = "\\.csv$")

# Read all datasets into a list
datasets <- lapply(files, read.csv)

# Check and convert 'price' and 'year' columns in each dataset
datasets <- lapply(datasets, function(df) {
  # Convert 'price' column to numeric
  df$price <- as.numeric(as.character(df$price))
  
  # Convert 'year' column to numeric
  df$year <- as.numeric(as.character(df$year))
  
  # Convert 'mileage' column to numeric if it's not already
  if (is.character(df$mileage)) {
    df$mileage <- as.numeric(as.character(df$mileage))
  }
  
  return(df)
})

# Combine all datasets into one dataframe
combined_data <- bind_rows(datasets)

# Clean the combined data to remove rows with NA values in 'price', 'year', or 'mileage'
cleaned_data <- combined_data %>%
  filter(!is.na(price) & !is.na(year) & !is.na(mileage))

# Check the structure of the cleaned data
str(cleaned_data)

# Perform Pearson's correlation test
correlation_result <- cor.test(cleaned_data$year, cleaned_data$price, method = "pearson")

# Print the Pearson correlation test results
print(correlation_result)

# Create a histogram of 'price' grouped by 'year'
ggplot(cleaned_data, aes(x = price, fill = as.factor(year))) +
  geom_histogram(binwidth = 1000, alpha = 0.6, position = "identity", color = "black") +
  labs(title = "Histogram of Price by Year",
       x = "Price",
       y = "Frequency",
       fill = "Year") +
  theme_minimal() +
  scale_fill_viridis_d()  # Adds a color palette for years

# Save the histogram as a PNG file
ggsave("histogram_price_by_year.png")

# Optional: Print the cleaned data (you can adjust the number of rows to print)
print(head(cleaned_data))

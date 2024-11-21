# Load necessary libraries
library(dplyr)
library(ggplot2) # For plotting

# Specify the directory where the datasets are stored
data_directory <- "G:/Hertfordshire Class/Team Research & Development/archive"

# List all CSV files in the directory
file_list <- list.files(path = data_directory, pattern = "*.csv", full.names = TRUE)

# Read, clean, and combine all datasets
all_datasets <- file_list %>%
  lapply(function(file) {
    data <- read.csv(file)
    data <- data %>%
      mutate(
        price = as.numeric(as.character(price)),     # Convert 'price' to numeric
        year = as.numeric(as.character(year)),       # Convert 'year' to numeric
        mileage = as.numeric(as.character(mileage)) # Convert 'mileage' to numeric
      )
    return(data)
  }) %>%
  bind_rows() # Combine all data frames into one

# Remove rows with missing or invalid values in 'year', 'price', or 'mileage'
cleaned_data <- all_datasets %>%
  filter(!is.na(year) & !is.na(price) & !is.na(mileage))

# Debugging: Check if cleaned_data has valid numeric values
cat("\nPreview of cleaned_data:\n")
print(head(cleaned_data))

cat("\nStructure of cleaned_data:\n")
str(cleaned_data)

# Check for any unexpected data in 'year' or 'price'
cat("\nSummary of 'year':\n")
print(summary(cleaned_data$year))

cat("\nSummary of 'price':\n")
print(summary(cleaned_data$price))

# Calculate Pearson's correlation between 'year' and 'price'
correlation <- cor(cleaned_data$year, cleaned_data$price, method = "pearson")

# Display the correlation result
cat("\nPearson's correlation coefficient between year and price is:", correlation, "\n")

# Perform statistical significance test
correlation_test <- cor.test(cleaned_data$year, cleaned_data$price, method = "pearson")

# Display the test result
cat("\nStatistical significance test results:\n")
print(correlation_test)

# Create a scatter plot with regression line
if (nrow(cleaned_data) > 1) {
  plot <- ggplot(cleaned_data, aes(x = year, y = price)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +
    labs(
      title = "Scatter Plot of Car Price vs. Year",
      x = "Year",
      y = "Price"
    ) +
    theme_minimal()
  
  # Display the plot
  print(plot)
} else {
  cat("\nNot enough data points to generate a plot.\n")
}

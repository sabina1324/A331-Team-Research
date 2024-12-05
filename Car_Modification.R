# Load necessary libraries
library(dplyr)
library(tidyr)  # For drop_na function
library(ggplot2)

# Set the working directory to the folder containing the CSV files
setwd("C:/Users/Windows/Desktop/Team Research")

# List all CSV files in the directory
csv_files <- list.files(pattern = "*.csv")

# Function to standardize column types
read_and_clean <- function(file) {
  data <- read.csv(file)
  # Ensure 'price', 'mileage', and 'year' are numeric/integer
  data$price <- as.numeric(data$price)
  data$mileage <- as.numeric(data$mileage)  # Convert mileage to numeric
  data$year <- as.integer(data$year)
  return(data)
}

# Read and combine all CSV files into a single data frame
combined_data <- lapply(csv_files, read_and_clean) %>%
  bind_rows()

# Check for NA values in the key columns
cat("NA values in year column:", sum(is.na(combined_data$year)), "\n")
cat("NA values in price column:", sum(is.na(combined_data$price)), "\n")
cat("NA values in mileage column:", sum(is.na(combined_data$mileage)), "\n")

# Remove rows with missing 'year', 'price', or 'mileage'
combined_data <- combined_data %>% drop_na(year, price, mileage)

# Create a scatter plot of Year vs Price
plot <- ggplot(combined_data, aes(x = year, y = price)) +
  geom_point() +
  labs(title = "Scatter Plot of Year vs Price",
       x = "Year",
       y = "Price")

# Display the plot
print(plot)

# Save the plot as a PNG file
ggsave("scatter_plot.png")

# Perform a simple linear regression analysis
model <- lm(price ~ year + mileage, data = combined_data)

# Display the summary of the regression model
summary(model)

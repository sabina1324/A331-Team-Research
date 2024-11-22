# Load necessary library
library(ggplot2)

# Set the directory path containing the datasets
data_path <- "G:\\Hertfordshire Class\\Team Research & Development\\archive"

# List all files in the directory
file_list <- list.files(data_path, full.names = TRUE)

# Initialize an empty data frame to combine all datasets
combined_data <- data.frame()

# Loop through each file and standardize the columns
for (file in file_list) {
  temp_data <- tryCatch({
    read.csv(file, header = TRUE)
  }, error = function(e) {
    message(paste("Error reading file:", file, " - ", e$message))
    return(NULL)
  })
  
  if (!is.null(temp_data)) {
    colnames(temp_data) <- tolower(colnames(temp_data))  # Convert column names to lowercase
    
    # Select only the 'year' and 'price' columns, handling missing columns
    temp_data <- temp_data[, c("year", "price")]
    
    # Convert 'price' to numeric, handling potential errors
    temp_data$price <- as.numeric(as.character(temp_data$price))
    temp_data <- temp_data[!is.na(temp_data$price), ]  # Remove rows with NA in 'price'
    
    # Combine the selected columns with the main data frame
    combined_data <- rbind(combined_data, temp_data)
  }
}

# Check if 'price' column exists and has valid data
if (!("price" %in% colnames(combined_data)) || nrow(combined_data) == 0) {
  stop("The 'price' column is missing or empty in the combined data.")
}

# Create the histogram
histogram_plot <- ggplot(combined_data, aes(x = price)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Car Prices", x = "Price", y = "Frequency") +
  theme_minimal()

# Print the histogram
print(histogram_plot)

# Save the histogram to a PNG file
png(filename = "price_histogram.png", width = 800, height = 600)
print(histogram_plot)
dev.off()

cat("Histogram saved as 'price_histogram.png' in the current working directory.\n")
setwd("C:/Users/Alif/Documents/A331-Team-Research/dataset")
getwd()
library(readr)
library(dplyr)
library(ggplot2)
setwd("C:/Users/Alif/Documents/A331-Team-Research/dataset")
file_list <- list.files(pattern = "*.csv")
data_list <- list()
clean_and_convert <- function(data) {
  columns_to_convert <- c("price", "engine size2")
  for (col in columns_to_convert) {
    if (col %in% names(data)) {
      data[[col]] <- as.numeric(gsub("[^0-9.]", "", data[[col]]))
    }
  }
  return(data)
}
for (i in seq_along(file_list)) {
    file_path <- file.path(getwd(), file_list[i])
  data <- read_csv(file_path)
  data <- clean_and_convert(data)
  data_list[[i]] <- data
}
combined_data <- bind_rows(data_list)
head(combined_data)
summary(combined_data)
ggplot(combined_data, aes(x = factor(year), y = price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  stat_summary(fun = median, geom = "point", color = "red", size = 2) +
  labs(title = "Boxplot of Price by Year", x = "Year", y = "Price") +
  theme_minimal()

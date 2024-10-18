getwd()
# Seting the working directory
setwd("G:/Semester 7/ca1-assignment-40-Ahmad-385")

# Loading libraries
install.packages("dplyr")
library(dplyr)

# Import the dataset
data <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", stringsAsFactors = FALSE)

# Checking the dataset was loaded correctly

# Display the first few rows
head(data)

# Displays the structure of the dataset
str(data)

# Print the dimensions of the dataset (rows and columns)
dim(data)

##TASK---1

# Counting missing values in each column
missing_per_column <- colSums(is.na(data))
print(missing_per_column)

# Count missing values in each row
missing_per_row <- rowSums(is.na(data))

# installing library to plot a graph
install.packages("ggplot2")
library(ggplot2)

# Count the number of missing columns (columns with at least one missing value)
missing_columns_count <- sum(colSums(is.na(data)) > 0)
print(paste("Number of columns with missing values:", missing_columns_count))

# Plot missing values of colomn
na_column_df <- data.frame(Column = names(missing_per_column), Missing = missing_per_column)
ggplot(na_column_df, aes(x = Column, y = Missing)) +
  geom_bar(stat = "identity", fill = "purple") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Missing Values Per Column", x = "Columns", y = "Count of Missing Values")

# Count the number of missing rows (rows with at least one missing value)
missing_rows_count <- sum(rowSums(is.na(data)) > 0)
print(paste("Number of rows with missing values:", missing_rows_count))


# Calculate missing values per row
missing_per_row <- rowSums(is.na(data))

# Create a dataframe for plotting, focusing only on rows with missing values
missing_row_df <- data.frame(Row = seq_along(missing_per_row), Missing = missing_per_row)
missing_row_df <- missing_row_df[missing_row_df$Missing > 0, ]

# Plot missing values per row
ggplot(missing_row_df, aes(x = Row, y = Missing)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Missing Values Per Row", x = "Row Index", y = "Count of Missing Values") +
  theme_minimal()


# Replace missing numeric values with mean
data_mean <- data
for (col in names(data_mean)) {
  if (is.numeric(data_mean[[col]])) {
    data_mean[[col]][is.na(data_mean[[col]])] <- mean(data_mean[[col]], na.rm = TRUE)
  }
}

# Replace missing numeric values with median
data_median <- data
for (col in names(data_median)) {
  if (is.numeric(data_median[[col]])) {
    data_median[[col]][is.na(data_median[[col]])] <- median(data_median[[col]], na.rm = TRUE)
  }
}



# Replace missing categorical values with mode
replace_mode <- function(x) {
  mode_val <- names(which.max(table(x, useNA = "no")))
  x[is.na(x)] <- mode_val
  return(x)
}

data_mode <- data
for (col in names(data_mode)) {
  if (is.character(data_mode[[col]])) {
    data_mode[[col]] <- replace_mode(data_mode[[col]])
  }
}

#Plot comparison of missing values before and after handling
missing_after_mean <- colSums(is.na(data_mean))
missing_after_median <- colSums(is.na(data_median))
missing_after_mode <- colSums(is.na(data_mode))

# Create data frame for visualization
missing_df <- data.frame(
  Method = rep(c("Mean", "Median", "Mode"), each = ncol(data)),
  Column = rep(names(data), 3),
  Missing = c(missing_after_mean, missing_after_median, missing_after_mode)
)

# Visualize missing values after imputation

ggplot(missing_df, aes(x = Column, y = Missing, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Missing Values After Imputation", x = "Columns", y = "Count of Missing Values")

# Verifing there is no missing values after cleanaing
sum(is.na(data_mean))  




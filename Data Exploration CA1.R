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

# Loading library
install.packages("e1071")
library(e1071)

#Skewness

# Ensure missing values are handled first
data_cleaned <- data
for (col in names(data_cleaned)) {
  if (is.numeric(data_cleaned[[col]])) {
    data_cleaned[[col]][is.na(data_cleaned[[col]])] <- mean(data_cleaned[[col]], na.rm = TRUE)
  }
}


# Calculate skewness for numeric columns
numeric_cols <- sapply(data_cleaned, is.numeric)  # Identify numeric columns
numeric_data <- data_cleaned[, numeric_cols]      # Subset numeric columns

# Compute skewness
skewness_values <- apply(numeric_data, 2, skewness)

# Display skewness values
print(skewness_values)

# Plot skewness as a barplot
barplot(skewness_values, main = "Skewness of Numeric Variables Before Outlier Removal",
        col = "pink", las = 2, xlab = "Variables", ylab = "Skewness")

# Identify numeric columns
numeric_cols <- sapply(data, is.numeric)
numeric_data <- data[, numeric_cols]

# Initialize a results list for summary
outlier_summary <- list()

# Loop through numeric columns to calculate outliers
for (col_name in names(numeric_data)) {
  # Extract column data
  col_data <- numeric_data[[col_name]]
  
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(col_data, 0.25, na.rm = TRUE)
  Q3 <- quantile(col_data, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Define bounds
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Identify outliers
  outliers <- col_data[col_data < lower_bound | col_data > upper_bound]
  
  # Store results in the list
  outlier_summary[[col_name]] <- list(
    Number_of_Outliers = length(outliers),
    Outliers = outliers
  )
  
  # Print summary for each column
  print(paste("Column:", col_name))
  print(paste("Number of outliers:", length(outliers)))
  print("Outliers:")
  print(outliers)
}

# Visualize outliers with boxplots for all numeric columns
par(mfrow = c(ceiling(length(numeric_data) / 2), 2))  # Adjust layout for boxplots
for (col_name in names(numeric_data)) {
  boxplot(numeric_data[[col_name]], main = paste("Boxplot of", col_name), col = "blue")
}




# Calculate Q1, Q3, and IQR for MonthlyCharges
Q1 <- quantile(data$MonthlyCharges, 0.25, na.rm = TRUE)
Q3 <- quantile(data$MonthlyCharges, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- data$MonthlyCharges[data$MonthlyCharges < lower_bound | data$MonthlyCharges > upper_bound]

# Print summary of outliers
print(paste("Number of outliers:", length(outliers)))
print("Outliers:")
print(outliers)

# Visualize data distribution with boxplot
boxplot(data$MonthlyCharges, main = "Boxplot of MonthlyCharges (Before Outlier Handling)", col = "purple")


# Function to detect and cap outliers using IQR
cap_outliers <- function(x) {
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Define lower and upper bounds
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  # Count outliers
  outliers <- sum(x < lower | x > upper, na.rm = TRUE)
  print(paste("Number of outliers detected:", outliers))
  
  # Replace outliers with bounds
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

# Apply the capping function to MonthlyCharges
data$MonthlyCharges <- cap_outliers(data$MonthlyCharges)

# Boxplot to visualize outliers after removal
boxplot(data$MonthlyCharges, main = "Boxplot After Outlier Removal", col = "grey")

# Summary statistics before and after outlier handling
summary_before <- summary(data$MonthlyCharges)
print("Summary Statistics After Outlier Removal:")
print(summary_before)


par(mfrow = c(1, 2))
# Set up two plots side by side  for easy overview
boxplot(data$MonthlyCharges, main = "Before Outlier Removal", col = "blue")
boxplot(cap_outliers(data$MonthlyCharges), main = "After Outlier Removal", col = "green")




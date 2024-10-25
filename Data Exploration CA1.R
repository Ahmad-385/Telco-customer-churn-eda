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



# Load the required libraries
install.packages("gridExtra")
library(gridExtra)


# Preview the dataset
head(data)

# Min-Max Scaling Function
normalizeMinMax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Min-Max Scaling for Numeric Columns
data_minmax <- data
numeric_cols <- sapply(data, is.numeric)
data_minmax[, numeric_cols] <- apply(data[, numeric_cols], 2, normalizeMinMax)

# Standardization Function
normalizeStandardized <- function(x) {
  return ((x - mean(x)) / sd(x))
}

# Standardization for Numeric Columns
data_standardized <- data
data_standardized[, numeric_cols] <- apply(data[, numeric_cols], 2, normalizeStandardized)

# Robust Scaling (Robust Standardization)
data_robust <- data
data_robust[, numeric_cols] <- scale(data[, numeric_cols], center = TRUE, scale = TRUE)

# Create histograms for the original and scaled data
options(repr.plot.width = 12, repr.plot.height = 4)

# Original Data
p1 <- ggplot(data, aes(x = MonthlyCharges)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black") +
  labs(title = "Histogram of Monthly Charges (Original Data)")

p2 <- ggplot(data_minmax, aes(x = MonthlyCharges)) +
  geom_histogram(binwidth = 0.05, fill = "green", color = "black") +
  labs(title = "Histogram of Monthly Charges (Min-Max Scaled)")

# Standardized Data
p3 <- ggplot(data_standardized, aes(x = MonthlyCharges)) +
  geom_histogram(binwidth = 0.2, fill = "red", color = "black") +
  labs(title = "Histogram of Monthly Charges (Standardized)")

# Robust Scaled Data
p4 <- ggplot(data_robust, aes(x = MonthlyCharges)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black") +
  labs(title = "Histogram of Monthly Charges (Robust Scaled)")

# Combine Histograms
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

#for total charges

# Check for missing values for totalcharges
sum(is.na(data$TotalCharges))

# Check for non-numeric values in total cahrges
non_numeric_values <- data$TotalCharges[!is.numeric(as.numeric(data$TotalCharges))]
print(non_numeric_values)
# Replace non-numeric or missing values with NA
data$TotalCharges <- as.numeric(data$TotalCharges)
data$TotalCharges[is.na(data$TotalCharges)] <- mean(data$TotalCharges, na.rm = TRUE)


# Min-Max Scaling Function
normalizeMinMax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Min-Max Scaling for TotalCharges
data_minmax <- data
data_minmax$TotalCharges <- normalizeMinMax(data$TotalCharges)

# Standardization Function
normalizeStandardized <- function(x) {
  return ((x - mean(x)) / sd(x))
}

# Standardization for TotalCharges
data_standardized <- data
data_standardized$TotalCharges <- normalizeStandardized(data$TotalCharges)

# Robust Scaling (Robust Standardization) for TotalCharges
data_robust <- data
data_robust$TotalCharges <- scale(data$TotalCharges, center = TRUE, scale = TRUE)

# Create histograms for the original and scaled data
options(repr.plot.width = 12, repr.plot.height = 4)

# Original Data
p1 <- ggplot(data, aes(x = TotalCharges)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  labs(title = "Histogram of Total Charges (Original Data)")

# Min-Max Scaled Data
p2 <- ggplot(data_minmax, aes(x = TotalCharges)) +
  geom_histogram(binwidth = 0.05, fill = "green", color = "black") +
  labs(title = "Histogram of Total Charges (Min-Max Scaled)")

# Standardized Data
p3 <- ggplot(data_standardized, aes(x = TotalCharges)) +
  geom_histogram(binwidth = 0.2, fill = "red", color = "black") +
  labs(title = "Histogram of Total Charges (Standardized)")

# Robust Scaled Data
p4 <- ggplot(data_robust, aes(x = TotalCharges)) +
  geom_histogram(binwidth = 0.2, fill = "purple", color = "black") +
  labs(title = "Histogram of Total Charges (Robust Scaled)")

# Combine Histograms
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)





#TASK 2 EDA &   Supporting Questions:

# Q1: What is the size of the dataset (number of rows and columns)?

print(dim(data))  # Rows and columns

#Q2: What are the data types and ranges of the columns?

print(str(data))  # Data types and structure

#Q3: Are there any missing values in the dataset?

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

# Visualize missing values
library(ggplot2)
na_column_df <- data.frame(Column = names(missing_values), Missing = missing_values)
ggplot(na_column_df, aes(x = Column, y = Missing)) +
  geom_bar(stat = "identity", fill = "brown") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Missing Values Per Column", x = "Columns", y = "Count of Missing Values")


#Q4: What is the distribution of MonthlyCharges, TotalCharges, and tenure?

# Distribution of numeric features
ggplot(data, aes(x = MonthlyCharges)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of MonthlyCharges", x = "MonthlyCharges", y = "Count")

ggplot(data, aes(x = TotalCharges)) +
  geom_histogram(binwidth = 100, fill = "green", color = "black") +
  labs(title = "Distribution of TotalCharges", x = "TotalCharges", y = "Count")

ggplot(data, aes(x = tenure)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black") +
  labs(title = "Distribution of Tenure", x = "Tenure", y = "Count")


#Q5: Are there any outliers in these features?


# Boxplots for outliers
boxplot(data$MonthlyCharges, main = "Boxplot of MonthlyCharges", col = "brown")
boxplot(data$TotalCharges, main = "Boxplot of TotalCharges", col = "pink")
boxplot(data$tenure, main = "Boxplot of Tenure", col = "grey")

#Q6: How does MonthlyCharges vary with Churn?

# Covariation between Churn and MonthlyCharges

ggplot(data, aes(x = Churn, y = MonthlyCharges, fill = Churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of MonthlyCharges by Churn", x = "Churn", y = "MonthlyCharges")

#Q7: Are customers with higher TotalCharges more likely to churn?

# Covariation between Churn and TotalCharges
ggplot(data, aes(x = Churn, y = TotalCharges, fill = Churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of TotalCharges by Churn", x = "Churn", y = "TotalCharges")

#Q8: Is there a relationship between tenure and Churn?

# Covariation between Churn and tenure
ggplot(data, aes(x = Churn, y = tenure, fill = Churn)) +
  geom_boxplot() +
  labs(title = "Boxplot of Tenure by Churn", x = "Churn", y = "Tenure")

#Q9: Are there customers with TotalCharges = 0 but with tenure > 0?

# Unusual patterns: Customers with TotalCharges = 0 but tenure > 0
unusual_patterns <- data[data$TotalCharges == 0 & data$tenure > 0, ]
print("Unusual Patterns:")
print(unusual_patterns)

#Q10: Are there clusters or gaps in MonthlyCharges or tenure?

# Scatterplot of tenure vs TotalCharges
ggplot(data, aes(x = tenure, y = TotalCharges, color = Churn)) +
  geom_point() +
  labs(title = "Scatterplot of Tenure vs TotalCharges", x = "Tenure", y = "TotalCharges")

mydata <- data
head(mydata)  # Display the first few rows

View(mydata)



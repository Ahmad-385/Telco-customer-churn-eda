# Telco Customer Churn – Exploratory Data Analysis (EDA)

This project focuses on performing **exploratory data analysis and data cleaning** on the Telco Customer Churn dataset. The aim is to understand the structure, detect quality issues, and prepare the dataset for future machine learning tasks such as churn prediction.

---

## 📂 Dataset

- **Source**: Telco Customer Churn dataset (`WA_Fn-UseC_-Telco-Customer-Churn.csv`)
- **Rows**: 7,043
- **Columns**: Customer demographics, account information, service usage, and churn labels.

---

## ✅ Key Objectives

- Understand data structure and types
- Identify and visualize missing data
- Handle missing values using imputation techniques
- Detect and visualize duplicates
- Analyze skewness of numerical features

---

## 🧠 Tasks Performed

1. **Initial Data Inspection**
   - `head()`, `str()`, and `dim()` to inspect structure and dimensions
2. **Missing Value Analysis**
   - Calculated missing values per column and row
   - Visualized missingness using bar charts
3. **Data Cleaning**
   - Imputed missing numeric values using **mean**
   - (Considered median and mode for alternatives)
   - Verified that no missing values remain after cleaning
4. **Skewness Detection**
   - Measured skewness for numeric features using `e1071` package
5. **Data Visualization**
   - Used `ggplot2` to create insightful bar charts

---

## 📊 Technologies Used

- **R**
- `dplyr` – data manipulation
- `ggplot2` – data visualization
- `e1071` – skewness analysis

---

## 📌 Project Status

✅ Completed EDA and data cleaning  
🚧 Ready for model training and churn prediction in future phases

---

## 📎 File Structure

- `Data Exploration CA1.R` – main R script for all EDA steps
- `WA_Fn-UseC_-Telco-Customer-Churn.csv` – dataset
- `Poster Presentation.pptx` – summary of findings (visual)
- `README.md` – project documentation

---

## 💬 Author

**Muhammad Ahmad**  
Final Year BSc (Hons) in Computing & IT  
CCT College Dublin  

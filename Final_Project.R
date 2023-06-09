# a) Load the dataset in R Studio. Examine the first few rows of data using R. Explain your findings. Did you notice anything abnormal or interesting?

# Load the dataset in to a variable named patients by keeping header as true so that the first row in csv file remains by not including it as values instead of attributes.
patients <- read.csv(file = "patients.csv", header = TRUE)

# Evaluating the dataset by performing some functions

# Checking the number of rows and columns in the provided "patients" dataset.
dim(patients)

# Looking at the starting few values
head(patients)
# We can see from the data that the attributes "BMI", and "Pedigree" are floating values, remaining are integer values.

# Looking at the bottom few values
tail(patients)
# Making sure that my interpretation regarding the dataset is right.

# Exploring the structure of dataset to better understand the variable types
str(patients)

# b) Provide summary statistics. Calculate the mean, median, standard deviation, and quartiles for each independent variable. Explain your results.
# Summary of the dataset
summary(patients)

# In the provided dataset, the dependent variable is the "Diagnosis" column. It represent the target variable that we are trying to predict.
# Independent variables are "Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "Pedigree", "Age".

# Calculating mean, median, standard deviation, and quartiles for each independent variable.

# Selecting only independent attributes
independent_attributes <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "Pedigree", "Age") 
dependent_attributes <- c("Diagnosis")

# Using subset operator to subset the original dataset
subset_attributes <- patients[, independent_attributes]

# Now calculating summary statistics for only independent variables/ attributes
head(subset_attributes)
summary(subset_attributes)
# summary() function only gives mean, median, and quartiles but not standard deviation. So use sd() to get standard deviation of all the independent variables.
standard_deviations <- apply(subset_attributes, 2, sd)
print(standard_deviations)
# we can see that the mean, median of "Glucose" attribute is higher compared to other attributes
# Whereas standard deviation is higher for "Insulin" attribute. 

# c) Using the ggplot2 library, create any five visualizations. Explain your reasoning for selecting those visualizations. Explain the output of each visualization. What are the insights your visualizations reveal about the dataset?

# Loads the ggplot2 to perform graphics
require(ggplot2)

# Creating 5 Visualizations
# 1. Histogram of glucose

ggplot(patients, aes(x = Glucose)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(x = "Glucose", y = "Frequency") +
  ggtitle("Histogram of Glucose")

# A histogram may be used to see how a numerical value, like glucose, is distributed. It displays the frequency of data fitting into various ranges or bins.
# Concentration of values mostly lie within 90 to 125 ranges.It is normally distributed and we can see some outliers at range 0 and 40.

# 2. Box Plot of Blood Pressure

ggplot(patients, aes(y = BloodPressure)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(y = "Blood Pressure") +
  ggtitle("Box Plot of Blood Pressure")

# The minimum, first quartile (Q1), median (Q2), third quartile (Q3), and maximum values of blood pressure in the dataset are shown in this box plot.
# The box plot reveals the center, spread and skewness of Blood Pressure. There are some outliers above and below the whiskers here.

# 3. Scatter Plot of BMI vs Age

ggplot(patients, aes(x = BMI, y = Age, color = BMI)) +
  geom_point() +
  labs(x = "BMI", y = "Age") +
  ggtitle("Scatter Plot of BMI vs. Age")

# The connection between two continuous variables, such as BMI and Age, may be shown using a scatter plot. Any patterns or connections between the variables can be found with its assistance.
# I can see that there is a non-linear relationship between BMI and Age. Additionally, it shows clusters among the data points between 20 to 40 BMI.

# 4. Bar Plot of Diagnosis

ggplot(patients, aes(x = factor(Diagnosis))) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(x = "Diagnosis", y = "Count") +
  ggtitle("Bar Plot of Diagnosis")

# This bar plot will show the count of each Diagnosis category (0 and 1) in the dataset.
# We can see from the bar plot that maximum number of pregnant women are not diagnosed.

# 5. Density Plot of Insulin

ggplot(patients, aes(x = Insulin)) +
  geom_density(fill = "steelblue", color = "black") +
  labs(x = "Insulin", y = "Density") +
  ggtitle("Density Plot of Insulin")

# A density plot shows how a continuous variable, such as insulin, has an estimated probability density function.
# The density is inversely proportion to Insulin as Insulin increases the density decreases.

# d) Find missing values for each independent variable and fill them with median values. The missing values for independent variables in the dataset are coded 0.

# Replace 0 with NA to convert all 0 values in the independent variables to NA to represent missing values.
# Replacing column names in the subset_attributes and accesing the variables using indexes.
modified_dataset <- subset_attributes

subset_attributes <- c(1,2,3,4,5,6,7,8)

for (attr in subset_attributes) {
  modified_dataset[modified_dataset[, attr] == 0, attr] <- NA
}

# Checking the dataset whether the missing values have been replaced to NA.

head(modified_dataset)

nomissing_dataset <- modified_dataset

# Now calculate the median for each independent variable.
median_values <- sapply(nomissing_dataset[, subset_attributes],      median, na.rm = TRUE)

print(median_values)



# After calculating the median values, now fill the missing values with median values.
for (attr in subset_attributes) {
  nomissing_dataset[is.na(nomissing_dataset[[attr]]), attr] <- median_values[attr]
} 

# Let's check whether the NA values have been replaced by median.
head(nomissing_dataset)

# e) Find outliers for each independent variable using the IQR rule.

# Using a loop to iterate over each independent variable and calculate the outliers based on IQR rule.

find_outliers <- function(column) {
  q1 <- quantile(column, 0.25)
  q3 <- quantile(column, 0.75)
  iqr <- q3 - q1
  lower_threshold <- q1 - 1.5 * iqr
  upper_threshold <- q3 + 1.5 * iqr
  outliers <- column[column < lower_threshold | column > upper_threshold]
  return(outliers)
}

# Find outliers for each independent variable
outliers <- lapply(nomissing_dataset, find_outliers)

# Print the outliers for each variable
for (i in seq_along(outliers)) {
  cat("Outliers for", names(outliers)[i], ":", toString(outliers[[i]]), "\n")
}

# f) Replace outliers. Explain your approach.

set.seed(123)
# install.packages("mice")
library(mice) 

replaced_outliers <- nomissing_dataset

# Calculate the median for each variable
median_values <- sapply(replaced_outliers, median, na.rm = TRUE)

# Loop through each variable and replace outliers with the median value
for (var in names(outliers)) {
  outliers_indices <- which(replaced_outliers[[var]] %in% outliers[[var]])
  replaced_outliers[outliers_indices, var] <- median_values[var]
}

head(replaced_outliers)

best_performance <- replaced_outliers
# g) Find the best performing variables/features using a correlogram.
# Install and load the corrplot package

# Install the corrplot package
#install.packages("corrplot")
library(corrplot)
## corrplot 0.92 loaded
# Compute the correlation matrix
cor_matrix <- cor(best_performance[, ])

cor_matrix

# Create the correlogram
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black")

# Compute pairwise correlations
cor_matrix <- cor(best_performance[, ], use = "pairwise.complete.obs")

# Find the absolute correlations for each variable
cor_abs <- apply(cor_matrix, 2, function(x) abs(x))

# Sort the variables based on the maximum correlation
best_variables <- names(sort(apply(cor_abs, 2, max), decreasing = TRUE))[1:5]

# Print the best performing variables
print(cat("Best performing variables:" , best_variables))

# h) Standardize your features to Gaussian distribution. Explain why it would be a good idea to standardize the features to Gaussian distribution.

standardized_variables <- best_variables

# Select only the variables in standardized_variables from your dataset
selected_data <- best_performance [, standardized_variables]

# Standardize the selected variables using scale()
standardized_data <- scale(selected_data)

# Update the standardized values in the original dataset
best_performance[, best_variables] <- standardized_data

best_performance






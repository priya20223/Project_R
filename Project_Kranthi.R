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
library(corrplot)
#correlation_matrix <- cor(best_performance)

# Select the variables of interest
variables_of_interest <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "Pedigree", "Age")

# Subset the dataset to include only the selected variables
subset_data <- best_performance[, variables_of_interest]

# Calculate the correlation matrix
correlation_matrix <- cor(subset_data)
correlation_matrix

#g)
# Install and load the ggplot2 package
#install.packages("ggplot2")
library(ggplot2)
library(corrplot)

# Create the correlation plot using a heatmap

#corrplot(correlation_matrix, method = "color")
#corMat = cor (best_performance[, -9])
corMat = cor (best_performance)
diag (corMat) = 0 #Remove self correlations
corrplot.mixed(corMat,tl.pos = "lt")
patients_2 <- best_performance

#h) Standardizing variables

# Standardize the selected variables to Gaussian distribution 
standardized_data <- scale(patients_2[, -9])

# Convert the standardized data back to a data frame 
standardized_data <- as.data.frame(standardized_data) 

# View the standardized data 
print(standardized_data) 

#h)
#Guassian distribution - density plots for standardized data features
plots <- lapply(standardized_data, function(var) { 
  ggplot(data = standardized_data, aes(x = var)) +
    geom_density(fill = "#DC143C", color = "black") +
    theme_minimal() +
    labs(x = "Standardized Value", y = "Density")
})
plots
# Arrange the density plots in a grid
#grid.arrange(grobs = plots, ncol = 3)

#h) Add a Diagnosis column to standardized_dataset 
standardized_data$Diagnosis <- patients_2$Diagnosis
standardized_data
#splitting the dataset into training and testing data (70:30)
#install.packages("caret")
library(caret)
set.seed(1234)
dindex <- createDataPartition(standardized_data$Diagnosis, p=0.7, list=FALSE)
train_data <- standardized_data[dindex,]
test_data <- standardized_data[-dindex,]
#i)Model_1 using correlation plot

best_features <- c( "BMI", "Age", "Pregnancies", "Glucose")
best_features_subset_data <- train_data[, c("Diagnosis", best_features)] 
#View(best_features_subset_data)

#LRM1 - using best features
LRM1 <- glm(Diagnosis ~ ., data = best_features_subset_data , family = binomial) 

# Print the summary of the model 
summary(LRM1)

#j & k
#install.packages("pROC")  # Install the pROC package
library(pROC)  # Load the pROC package
predictions <- predict(LRM1, newdata = test_data, type = "response")
predicted.classes <- ifelse(predictions> 0.5, 1, 0)
predicted.classes

library(caret)
confusion_matrix <- table(predicted.classes, test_data$Diagnosis)
confusion_matrix

precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * precision * recall / (precision + recall)
support <- rowSums(confusion_matrix)

# Print the classification report
cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1-Score: ", f1_score, "\n")
cat("Support: ", support)

#l
#accuracy of model1- LRM1 
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy: ", accuracy, "\n")
#m)model 2
#LRM2 - using all features
LRM2 <- glm(Diagnosis ~ ., data = train_data , family = binomial) 
#print the summary of the model
summary(LRM2)

#m)
predictions_model2 <- predict(LRM2, newdata = test_data, type = "response")
predicted.classes_model2 <- ifelse(predictions_model2> 0.5, 1, 0)
predicted.classes_model2

library(caret)
confusion_matrix_model2 <- table(predicted.classes_model2, test_data$Diagnosis)
confusion_matrix_model2

precision <- confusion_matrix_model2[2, 2] / sum(confusion_matrix_model2[, 2])
recall <- confusion_matrix_model2[2, 2] / sum(confusion_matrix_model2[2, ])
f1_score <- 2 * precision * recall / (precision + recall)
support <- rowSums(confusion_matrix_model2)

# Print the classification report
cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1-Score: ", f1_score, "\n")

#Accuracy of model2 - lRM2
accuracy <- sum(diag(confusion_matrix_model2)) / sum(confusion_matrix_model2)
cat("Accuracy: ", accuracy, "\n")






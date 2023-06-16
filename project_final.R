#a
#setting library
#setwd("~/Desktop/SIP690/Project_R")
#load data
patients <- read.csv("patients.csv")
#head 
head(patients, n=30)
#b)
#Summary statistics
summary(patients[, c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "Pedigree", "Age")]) 

#Calculate mean, median, standard deviation, and quartiles for each variable 
summary_stats <- data.frame( 
  Variable = names(patients), 
  Mean = colMeans(patients), 
  Median = apply(patients, 2, median), 
  Standard_Deviation = apply(patients, 2, sd), 
  Q1 = apply(patients, 2, quantile, 0.25), 
  Q3 = apply(patients, 2, quantile, 0.75) 
) 

print(summary_stats) 

#c visualizations
#1.distribution of glucose levels among the patients
library(ggplot2) 

ggplot(data = patients, aes(x = Glucose)) + 
  geom_histogram(binwidth = 10, fill = "#7463ac", color = "white") + 
  labs(title = "Distribution of Glucose levels", 
       x = "Glucose Levels", 
       y = "Frequency") 

#2.Distribution of BMI between patients with and without a diabetes diagnosis
ggplot(data = patients, aes(x = as.factor(Diagnosis), y = BMI, fill = as.factor(Diagnosis))) + 
  geom_boxplot() + 
  labs(title = "Boxplot of BMI and Diabetes Diagnosis", 
       x = "Diabetes Diagnosis", 
       y = "BMI") 
#3.relationship between age and blood pressure.
ggplot(data = patients, aes(x = Age, y = BloodPressure, color = as.factor(Diagnosis))) + 
  geom_point() + 
  labs(title = "Scatterplot of Age and Blood Pressure", 
       x = "Age", 
       y = "Blood Pressure") 
#4.Relationship between BMI and Age
ggplot(patients, aes(x = BMI, y = Age, color = BMI)) +
  geom_point() +
  labs(x = "BMI", y = "Age") +
  ggtitle("Scatter Plot of BMI vs. Age")
#5.Count or proportion of patients with and without a diabetes diagnosis
ggplot(data = patients, aes(x = as.factor(Diagnosis), fill = as.factor(Diagnosis))) + 
  geom_bar() + 
  labs(title = "Bar Plot of Diabetes Diagnosis", 
       x = "Diabetes Diagnosis", 
       y = "Count") 
patients_1 <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "Pedigree", "Age")
```


```{r}
# d)
#Iterate over each column (excluding the "Diagnosis" column)
for (col in names(patients_1)[-which(names(patients) == "Diagnosis")]) {
  # Find the indices of missing values (coded as 0)
  missing_indices <- which(patients[[col]] == 0)
  
  # Calculate the median excluding the missing values
  median_value <- median(patients[[col]][patients[[col]] != 0], na.rm = TRUE)
  
  # Replace the missing values with the median
  patients[missing_indices, col] <- median_value
}
patients
#d)
# Now calculate the median for each independent variable.

median_values <- sapply(patients[, patients_1], median, na.rm = TRUE)
print(median_values)
# After calculating the median values, now fill the missing values with median values.
for (attr in patients_1) {
  
  patients[is.na(patients[[attr]]), attr] <- median_values[attr]
  
}
# Let's check whether the NA values have been replaced by median.
head(patients)

#e)
#Define a function to detect outliers using the IQR rule 
detect_outliers <- function(x) { 
  q1 <- quantile(x, 0.25) 
  q3 <- quantile(x, 0.75) 
  iqr <- q3 - q1 
  lower_bound <- q1 - 1.5 * iqr 
  upper_bound <- q3 + 1.5 * iqr 
  outliers <- x[x < lower_bound | x > upper_bound] 
  return(outliers) 
} 

# Find outliers for each independent variable 
outliers <- lapply(patients[, -which(names(patients) == "Diagnosis")], detect_outliers) 

# Print the outliers for each variable 
for (i in seq_along(outliers)) { 
  variable <- names(patients[, -which(names(patients) == "Diagnosis")])[i] 
  cat("Outliers for", variable, ":") 
  if (length(outliers[[i]]) == 0) { 
    cat(" None\n") 
  } else { 
    cat("\n") 
    print(outliers[[i]]) 
  } 
} 

patients_2 <- patients
summary(patients_2)

#f) Approach for replacing outlier with lower and upper bound
for (i in seq_along(outliers)) { 
  variable <- names(patients_2[, -which(names(patients_2) == "Diagnosis")])[i] 
  
  if (variable != "Diagnosis" && length(outliers[[i]]) > 0) { 
    q1 <- quantile(patients_2[[variable]], 0.25) 
    q3 <- quantile(patients_2[[variable]], 0.75) 
    iqr <- q3 - q1 
    lower_bound <- q1 - 1.5 * iqr 
    upper_bound <- q3 + 1.5 * iqr 
    outliers_indices <- which(patients_2[[variable]] %in% outliers[[i]]) 
    
    for (index in outliers_indices) { 
      if (patients_2[[variable]][index] < lower_bound || patients_2[[variable]][index] > upper_bound) { 
        if (patients_2[[variable]][index] < lower_bound) { 
          patients_2[[variable]][index] <- max(patients_2[[variable]][patients_2[[variable]] >= lower_bound]) 
        } else { 
          patients_2[[variable]][index] <- min(patients_2[[variable]][patients_2[[variable]] <= upper_bound]) 
        } 
      } 
    } 
  } 
} 
summary(patients_2)

#g) Best performing variables 
#install.packages("corrplot")
library(corrplot)
#correlation_matrix <- cor(patients_2)

# Select the variables of interest
variables_of_interest <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "Pedigree", "Age")

# Subset the dataset to include only the selected variables
subset_data <- patients_2[, variables_of_interest]

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
#corMat = cor (patients_2[, -9])
corMat = cor (patients_2)
diag (corMat) = 0 #Remove self correlations
corrplot.mixed(corMat,tl.pos = "lt")

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

#i) 
#splitting the dataset into training and testing data (70:30)
#install.packages("caret")
library(caret)
set.seed(1234)
dindex <- createDataPartition(standardized_data$Diagnosis, p=0.7, list=FALSE)
train_data <- standardized_data[dindex,]
test_data <- standardized_data[-dindex,]


#model3
features_model3 <- c( "BMI", "Pedigree", "Glucose", "Age")
model3_features_subset_data <- train_data[, c("Diagnosis", features_model3)] 

#LRM3
LRM3 <- glm(Diagnosis ~ ., data = model3_features_subset_data , family = binomial) 

# Print the summary of the model 
summary(LRM3)


predictions_model3 <- predict(LRM3, newdata = test_data, type = "response")
predicted.classes_model3 <- ifelse(predictions_model3> 0.5, 1, 0)
predicted.classes_model3

library(caret)
confusion_matrix_model3 <- table(predicted.classes_model3, test_data$Diagnosis)
confusion_matrix_model3

accuracy <- sum(diag(confusion_matrix_model3)) / sum(confusion_matrix_model3)
precision <- confusion_matrix_model3[2, 2] / sum(confusion_matrix_model3[, 2])
recall <- confusion_matrix_model3[2, 2] / sum(confusion_matrix_model3[2, ])
f1_score <- 2 * precision * recall / (precision + recall)
support <- rowSums(confusion_matrix_model3)

# Print the classification report
cat("Accuracy: ", accuracy, "\n")
cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1-Score: ", f1_score, "\n")
cat("Support: ", support)
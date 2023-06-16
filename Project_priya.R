#Setting the directory
setwd("~/Downloads")
#Import  dataset
#a)Load the dataset in R Studio. Examine the first few rows of data using R. Explain your findings. Did you notice anything abnormal or interesting
patients <-read.csv("patients.csv")
head(patients, n=10)
#b)Provide summary statistics. Calculate the mean, median, standard deviation, and quartiles for each independent variable. Explain your results
#summary("Pregnancies", "Glucose")
        #, BloodPressure, SkinThickness, Insulin, BMI,Pedigree, Age)
summary(patients[, c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "Pedigree", "Age")])
summary_stats <- data.frame( Variable = names(patients), Mean = colMeans(patients), Median = apply(patients, 2, median), Standard_Deviation = apply(patients, 2, sd), Q1 = apply(patients, 2, quantile, 0.25), Q3 = apply(patients, 2, quantile, 0.75) )

print(summary_stats)
library(ggplot2)

ggplot(data = patients, aes(x = Glucose)) + geom_histogram(binwidth = 10, fill = "steelblue", color = "white") + labs(title = "Histogram of Glucose Levels", x = "Glucose Levels", y = "Frequency")

ggplot(data = patients, aes(x = as.factor(Diagnosis), y = BMI, fill = as.factor(Diagnosis))) + geom_boxplot() + labs(title = "Boxplot of BMI and Diabetes Diagnosis", x = "Diabetes Diagnosis", y = "BMI")

ggplot(data = patients, aes(x = Age, y = BloodPressure, color = as.factor(Diagnosis))) + geom_point() + labs(title = "Scatterplot of Age and Blood Pressure", x = "Age", y = "Blood Pressure")

ggplot(data = patients, aes(x = as.factor(Diagnosis), fill = as.factor(Diagnosis))) + geom_bar() + labs(title = "Bar Plot of Diabetes Diagnosis", x = "Diabetes Diagnosis", y = "Count")

missing_values <- sapply(patients[, -which(names(patients) == "Diagnosis")], function(x) sum(x == 0))

for (col in names(patients)[-which(names(patients) == "Diagnosis")]) { patients[patients[[col]] == 0, col] <- median(patients[[col]], na.rm = TRUE) }

missing_values_filled <- sapply(patients[, -which(names(patients) == "Diagnosis")], function(x) sum(x == 0))


print("Number of missing values before filling:") 
print(missing_values) 
print("Number of missing values after filling:")

print(missing_values_filled)
#Compare the original and filled dataset
comparison <- data.frame( Variable = names(patients[, -which(names(patients) == "Diagnosis")]), Original_Values = sapply(patients[, -which(names(patients) == "Diagnosis")], function(x) sum(x == 0)), Filled_Values = sapply(patients[, -which(names(patients) == "Diagnosis")], function(x) sum(x == median(x))) )

print(comparison)

detect_outliers <- function(x) { q1 <- quantile(x, 0.25) q3 <- quantile(x, 0.75) iqr <- q3 - q1 lower_bound <- q1 - 1.5 * iqr upper_bound <- q3 + 1.5 * iqr outliers <- x[x < lower_bound | x > upper_bound] return(outliers) }
cor_matrix <- cor(patients[, -which(names(patients) == "Diagnosis")])
cor_matrix
library(ggplot2)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black")


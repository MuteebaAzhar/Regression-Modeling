###### Necessary Packages and Libraries#########
# Install and load ggplot2 package for data visualization
install.packages("ggplot2")
library(ggplot2)

# Install and load dplyr package for data manipulation
install.packages("dplyr")
library(dplyr)

# Install and load pROC package for ROC curve analysis
install.packages("pROC")
library(pROC)

# Important Steps Before Data Cleaning:
# Set the file as the Working Directory
# Calling the Dataset
library(readr)
data<- read_csv("C:/Users/PMLS/Downloads/smoking_dataset.csv")
View(data)
data <- read.csv("smoking_dataset.csv")

###### Data Prepration #######
# To Check the Structure of Data
str(data)

# To Summarize the Data
summary(data)

# Rectification/ Discretization
data$gender <- as.factor(data$gender)
data$`hearing(left)`<- as.factor(data$`hearing(left)`)
data$`hearing(right)` <- as.factor(data$`hearing(right)`)
data$`Urine protein`<- as.factor(data$`Urine protein`)
data$oral <- as.factor(data$oral)
data$tartar <- as.factor(data$tartar)
data$`dental caries` <- as.factor(data$`dental caries`)
data$smoking <- as.factor(data$smoking)

# To Summarize the Discretized Data
summary(data)

# To Check the Structure of Discretize Data
str(data)


######## Data Cleaning ###########

# (a) Missing Values:

# Check for missing values
is.na(data)
# or
missing_values <- colSums(is.na(data))
# Display the count of missing values
print(missing_values)

# Handling missing values
na.omit(data)
# or
# Remove rows with missing values
data_cleaned <- data[complete.cases(data), ]
# Remove columns with missing values
data_cleaned <- data[, colSums(is.na(data)) == 0]
# Display the dimensions of the cleaned data
dim(data_cleaned)

# (b) Identifying and Handling Outliers:

# 1st Method:
# Identification
ggplot(data, aes(y = age)) + geom_boxplot()
# Removing outliers:
Q1 <- quantile(data$age, 0.25)
Q3 <- quantile(data$age, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.2 * IQR
upper_bound <- Q3 + 1.2 * IQR
data_no_outliers <- subset(data, age >= lower_bound & age <= upper_bound)
data_no_outliers

# 2nd Method:
# Identify outliers using the IQR method for 'eyesight.left.'
eyesight_left_outliers <- boxplot.stats(data$`eyesight(left)`)$out
# Display identified outliers
print(paste("Outliers in 'eyesight.left.':", toString(eyesight_left_outliers)))
# Set a threshold for potential outliers (adjust as needed)
outlier_threshold_left <- 1.5
# Calculate the interquartile range (IQR) for 'eyesight.left.'
iqr_left <- IQR(data$`eyesight(left)`)
# Define the lower and upper bounds for potential outliers
lower_bound_left <- quantile(data$`eyesight(left)`)[2] - outlier_threshold_left * iqr_left
upper_bound_left <- quantile(data$`eyesight(left)`)[4] + outlier_threshold_left * iqr_left
# Filter out potential outliers from 'data' for 'eyesight.left.'
data_cleaned_left <- data[!(data$`eyesight(left)` < lower_bound_left | data$`eyesight(left)` > upper_bound_left), ]


##### DESCRIPTIVE Statistics #########
# (a) Summary of Data Without Outliers for Numeric Variables:

# i-Age:
summary(data_cleaned_age$age)

# ii-Height:
summary(data_cleaned_height$height.cm.)

# iii-Weight:
summary(data_cleaned_weight$weight.kg.)

# iv-Waist:
summary(data_cleaned_waist$waist.cm.)

# v-Eyesight Left:
summary(data_cleaned_left$eyesight.left.)

# vi-Eyesight Right:
summary(data_cleaned_right$eyesight.right.)

# vii-Systolic:
summary(data_cleaned_systolic$systolic)

# viii-Relaxation:
summary(data_cleaned_relaxation$relaxation)

# ix-Fasting Blood Sugar:
summary(data_cleaned_fbs$fasting.blood.sugar)

# x-Cholesterol:
summary(data_cleaned_cholesterol$Cholesterol)

# xi-Triglyceride:
summary(data_cleaned_triglyceride$triglyceride)

# xii-HDL:
summary(data_cleaned_hdl$HDL)

# xiii-LDL:
summary(data_cleaned_ldl$LDL)

# xiv-Hemoglobin:
summary(data_cleaned_hemoglobin$hemoglobin)

# xv-Serum Creatinine:
summary(data_cleaned_serum_creatinine$serum.creatinine)

# xvi-AST:
summary(data_cleaned_ast$AST)

# xvii-ALT:
summary(data_cleaned_alt$ALT)

# xviii-GTP:
summary(data_cleaned_gtp$Gtp)

# (b) Understanding the Distribution of Variables:

# Data distribution among numeric variables after the removal of outliers by using Histograms and Box plots:

# i-Age:
# Create a histogram of 'age'
hist(data_cleaned_age$age,
     main = "Histogram of Age",
     xlab = "Age",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

# Create a boxplot of 'age'
boxplot(data_cleaned_age$age,
        main = "Boxplot of Age",
        ylab = "Age")

# ii-Height:
# Create a histogram of 'Height'
hist(data_cleaned_height$`height.cm.`,
     main = "Histogram of Height",
     xlab = "Height",
     ylab = "Frequency",
     col = "purple",
     border = "black")

# Create a boxplot of 'Height'
boxplot(data_cleaned_height$`height.cm.`,
        main = "Boxplot of Height",
        ylab = "Height")

# iii-Weight:
# Create a histogram of 'Weight'
hist(data_cleaned_weight$`weight.kg.`,
     main = "Histogram of Weight",
     xlab = "Weight",
     ylab = "Frequency",
     col = "red",
     border = "black")

# Create a boxplot of 'Weight'
boxplot(data_cleaned_weight$`weight.kg.`,
        main = "Boxplot of Weight",
        ylab = "Weight")


# Barplot for Categoric value:
# Bar plot for Smoking Status
ggplot(data_no_outliers, aes(x = factor(smoking))) +
  geom_bar(fill = "red", alpha = 0.7) +
  labs(title = "Bar Plot for Smoking Status", x = "Smoking Status", y = "Frequency")

# Contingency table for Smoking Status
contingency_table_smoking <- table(data_no_outliers$smoking)
print("Contingency Table for Smoking Status:")
print(contingency_table_smoking)

###### Contingency Tables ###########
# Smoking:
print("Contingency Table for Smoking Status:")
print(contingency_table_smoking)

# Gender:
contingency_table_gender <- table(data_no_outliers$gender)
print("Contingency Table for Gender:")
print(contingency_table_gender)

# Hearing Left:
contingency_table_hearing_left <- table(data_no_outliers$hearing.left.)
print("Contingency Table for Hearing Left:")
print(contingency_table_hearing_left)

# Hearing Right:
contingency_table_hearing_right <- table(data_no_outliers$hearing.right.)
print("Contingency Table for Hearing Right:")
print(contingency_table_hearing_right)

# Urine protein:
contingency_table_urine_protein <- table(data_no_outliers$Urine.protein)
print("Contingency Table for Urine protein:")
print(contingency_table_urine_protein)

# Dental Caries:
contingency_table_dental_caries <- table(data_no_outliers$dental.caries)
print("Contingency Table for Dental Caries:")
print(contingency_table_dental_caries)

# Tartar:
contingency_table_tartar <- table(data_no_outliers$tartar)
print("Contingency Table for Tartar:")
print(contingency_table_tartar)


# Barplot for Categoric value:
# Bar plot for Smoking Status
ggplot(data_no_outliers, aes(x = factor(smoking))) +
  geom_bar(fill = "red", alpha = 0.7) +
  labs(title = "Bar Plot for Smoking Status", x = "Smoking Status", y = "Frequency")

# Contingency table for Smoking Status
contingency_table_smoking <- table(data_no_outliers$smoking)
print("Contingency Table for Smoking Status:")
print(contingency_table_smoking)

# Contingency Tables:
# Smoking:
print("Contingency Table for Smoking Status:")
print(contingency_table_smoking)

# Gender:
contingency_table_gender <- table(data_no_outliers$gender)
print("Contingency Table for Gender:")
print(contingency_table_gender)

# Hearing Left:
contingency_table_hearing_left <- table(data_no_outliers$hearing.left.)
print("Contingency Table for Hearing Left:")
print(contingency_table_hearing_left)

# Hearing Right:
contingency_table_hearing_right <- table(data_no_outliers$hearing.right.)
print("Contingency Table for Hearing Right:")
print(contingency_table_hearing_right)

# Urine protein:
contingency_table_urine_protein <- table(data_no_outliers$Urine.protein)
print("Contingency Table for Urine protein:")
print(contingency_table_urine_protein)

# Dental Caries:
contingency_table_dental_caries <- table(data_no_outliers$dental.caries)
print("Contingency Table for Dental Caries:")
print(contingency_table_dental_caries)

# Tartar:
contingency_table_tartar <- table(data_no_outliers$tartar)
print("Contingency Table for Tartar:")
print(contingency_table_tartar)



######### Explorarotory data analysis ########
# Visualizations to understand the Distribution of key variables with Smoking Variable:

# Bar Plots: (To analyze the relation between two categorical variables)

# Contingency Table for Smoking and Gender
abc_gender <- table(data_no_outliers$smoking, data_no_outliers$gender)
print("Contingency Table for Smoking and Gender:")
print(abc_gender)

# Contingency Table for Smoking and Hearing Left
abc_hearing_left <- table(data_no_outliers$smoking, data_no_outliers$`hearing(left)`)
print("Contingency Table for Smoking and Hearing Left:")
print(abc_hearing_left)

# Contingency Table for Smoking and Hearing Right
abc_hearing_right <- table(data_no_outliers$smoking, data_no_outliers$`hearing(right)`)
print("Contingency Table for Smoking and Hearing Right:")
print(abc_hearing_right)

# Contingency Table for Smoking and Urine Protein
abc_urine_protein <- table(data_no_outliers$smoking, data_no_outliers$`Urine protein`)
print("Contingency Table for Smoking and Urine Protein:")
print(abc_urine_protein)

# Contingency Table for Smoking and Dental Caries
abc_dental_caries <- table(data_no_outliers$smoking, data_no_outliers$`dental caries`)
print("Contingency Table for Smoking and Dental Caries:")
print(abc_dental_caries)

# Contingency Table for Smoking and Tartar
abc_tartar <- table(data_no_outliers$smoking, data_no_outliers$tartar)
print("Contingency Table for Smoking and Tartar:")
print(abc_tartar)

# Box Plots: (To analyze relation between a numeric & categoric variable)

# Assuming data_cleaned_age is your cleaned dataset
# Boxplot of age by Smoking Status
ggplot(data_cleaned_age[complete.cases(data_cleaned_age$age), ], aes(x = smoking, y = age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Smoking Status", x = "Smoking Status", y = "Age")

# or

ggplot(data_no_outliers, aes(x = smoking, y = age)) +
  geom_boxplot(fill = 'red', color = 'black', alpha = 0.7) +
  theme_minimal()


########## Regression Modeling #######

# Split the data into training and testing sets
set.seed(123) # Set a seed for reproducibility
sample_index <- sample(1:nrow(data_no_outliers), 0.7 * nrow(data_no_outliers))
train_data <- data_no_outliers[sample_index, ]
test_data <- data_no_outliers[-sample_index, ]

# Build logistic regression model using training data
logistic_model <- glm(smoking ~ height.cm. + weight.kg. + triglyceride + hemoglobin +
                        ALT + Gtp + tartar + gender + dental.caries, 
                      data = train_data, family = "binomial")

# Display the summary of the model
summary(logistic_model)

# Make predictions on the test set
predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Convert predicted probabilities to binary outcomes
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Create a confusion matrix (Error Analysis)
conf_matrix <- table(predicted_classes, test_data$smoking)
print(conf_matrix)

# Calculate accuracy, precision, recall, and F1 score
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Total Number of Observations
cat("Number of observations:", nrow(test_data), "\n")

# Number of Predictive Positive Observations
cat("Number of predicted positive:", sum(predicted_classes), "\n")

# Number of Actual Positive Observations
cat("Number of actual positive:", sum(test_data$smoking == 1), "\n")

# Build ROC curve
roc_curve <- roc(test_data$smoking, predictions)
auc_value <- auc(roc_curve)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
text(0.8, 0.2, paste("AUC =", round(auc_value, 2)), col = "red", cex = 1.2)





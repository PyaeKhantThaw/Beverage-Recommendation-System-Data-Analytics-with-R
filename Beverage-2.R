library(tidyverse)
library(dplyr)
library(ggplot2)
library(explore)
library(tidyr)
library(caret)
setwd("C:/Users/MIIT/Desktop/DA")
coffee <- read.csv("Beverage.csv")

View(coffee)

###(A)-->RevieW Dataset file

#dimension 
dim(coffee)

#first 6 rows
head(coffee)

#last 6 rows 
tail(coffee)

#structure 
str(coffee)

#display Beverages
unique(coffee$Beverage)

#display Summary
summary(coffee)

###(B) --> Which pre-processing steps would be required to prepare 
#this dataset for analysis?   Normalize your data.
# Check for missing values
missing_values <- sum(is.na(coffee))
print(missing_values)

View(coffee)

# Replacing null with mean value(if needed)
mean_Caffeine <- mean(coffee$Caffeine, na.rm = TRUE)
mean_Caffeine
coffee$Caffeine[is.na(coffee$Caffeine)] <- mean_Caffeine
coffee$Caffeine

# Check for duplicates
sum(duplicated(coffee))
summary(coffee)

# Remove duplicate rows
coffee <- coffee[!duplicated(coffee), ]

#--> Normalize the data 
# Select only numeric columns for normalization
numeric_cols <- coffee %>% select_if(is.numeric)
View(numeric_cols)
# Min-Max normalization function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
# Apply normalization to numeric columns
normalized_data <- numeric_cols %>% mutate_all(normalize)
head(normalized_data)

###(C) -->Identify a research question that could be asked based on your data.
#1-->"Can we predict the calorie content of a beverage based on 
#its nutritional properties?"
summarise(coffee, correlation = cor(x = Calories, y = Total.Fat ))
summarise(coffee, correlation = cor(x = Calories , y = Protein ))
summarise(coffee, correlation = cor(x = Calories , y = Carbohydrates ))
summarise(coffee, correlation = cor(x = Calories , y = Cholesterol ))
#2--> "Does the amount of sugar in a beverage correlate with its calories?"
summarise(coffee, correlation = cor(x = Calories , y = Sugar))

###(D) -->Visualize the distribution of each variable and check 
#the relationships between variables, and potential outliers
# Histograms to check distribution
ggplot(coffee, aes(x = Calories)) + 
  geom_histogram(binwidth = 8, fill ='orange', color = 'black') + 
  labs(title = "Distribution of Calories")
ggplot(coffee, aes(x = Total.Fat)) + 
  geom_histogram(binwidth = 0.5, fill = 'yellow', color = 'pink') +
  labs(title = "Distribution of Total.fat")
ggplot(coffee, aes(x = Caffeine)) + 
  geom_histogram(binwidth = 10 ,fill ='blue' ,color = 'red') +
  labs(title = "Distribution of Caffeine")
ggplot(coffee, aes(x = Cholesterol)) + 
  geom_histogram(binwidth = 3,fill ='violet' ,color = 'white') +
  labs(title = "Distribution of Cholesterol")
ggplot(coffee, aes(x = Sugar)) + 
  geom_histogram(binwidth = 3,fill ='white' ,color = 'green') +
  labs(title = "Distribution of Sugar")
#Categorical vars
ggplot(coffee, aes(x = Calcium, fill = Calcium)) + geom_bar() +
  labs(title = "Distribution of Calcium")
ggplot(coffee, aes(x = Vitamin.A, fill = Vitamin.A)) + geom_bar() +
  labs(title = "Distribution of Vitamin A")
ggplot(coffee, aes(x = Iron, fill = Iron)) + geom_bar() +
  labs(title = "Distribution of Iron")

# Visualize relationships between variables using scatter plots
ggplot(coffee, aes(x = Cholesterol, y = Calories)) + geom_point() +
  labs(title = "Calories vs Cholesterol")

ggplot(coffee, aes(x = Calories, y = Carbohydrates)) + geom_point() +
  labs(title = "Calories vs Carbohydrates")

ggplot(coffee, aes(x = Calories, y = Sugar)) + geom_point() +
  labs(title = "Calories vs Sugar")

# Pair plot to visualize relationships and distributions
pairs(coffee[, c("Calories", "Total.Fat", "Carbohydrates",
                 "Cholesterol", "Sugar", "Protein")])

# Pairwise scatter plots with color differentiation for each beverage
ggplot(coffee, aes(x = Sugar, y = Calories, color = Beverage)) + geom_point()

# Box plots to visualize distribution and potential outliers
ggplot(coffee, aes(x = 1, y = Calories)) + 
  geom_boxplot(fill = "lightblue", color = "green") +
  labs(title = "Box Plot of Calories ")
ggplot(coffee, aes(x = 1, y = Total.Fat)) + 
  geom_boxplot(fill = "lightblue", color = "green") +
  labs(title = "Box Plot of Total.Fat ")
ggplot(coffee, aes(x = 1, y = Caffeine)) + 
  geom_boxplot(fill = "lightblue", color = "green") +
  labs(title = "Box Plot of Caffeine ")

###(E) --> Determine the independent variable and 
#dependent variables according to the Research question.
#Research Questions
#1-->"Can we predict the calorie content of a beverage based on 
#its nutritional properties?"

#2--> "Does the amount of sugar in a beverage correlate with its calories?"
independent_vars <- coffee[, c("Cholesterol","Carbohydrates","Protein", "Sugar")]
dependent_var <- coffee$Calories
View(independent_vars)

###(F) -->Split your dataset into two or three parts: Training Data,
#Validation Data (optional), Test Data
coffee <- normalized_data 
# Select relevant variables
selected_vars <- coffee %>%
  select(Cholesterol,Carbohydrates,Protein,Sugar,Calories)
View(selected_vars)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(selected_vars$Calories, p = 0.8, list = FALSE)
train_data <- selected_vars[train_index, ]
test_data <- selected_vars[-train_index, ]

###(G) -->Build an appropriate Regression Model for prediction 
          #according to the Research question and test your model

# Build a linear regression model
model <- lm(Calories ~ Cholesterol + Protein + Carbohydrates + Sugar, data= train_data)
# Print the summary of the model
summary(model)

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

###(I)Assess the model's performance using appropriate evaluation metrics.
# Evaluate the model
rmse <- sqrt(mean((test_data$Calories - predictions)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Calculate Mean Absolute Error (MAE) if needed
mae <- mean(abs(test_data$Calories - predictions))
cat("Mean Absolute Error (MAE):", mae, "\n")

mean(coffee$Calories)
print(predictions)

###(J)Display  to know how differ the Predicted value and Observed value.
plot(test_data$Calories, predictions, main = "Actual vs. Predicted Calories",
     xlab = "Actual Calories", ylab = "Predicted Calories")
abline(0, 1, col = "green")

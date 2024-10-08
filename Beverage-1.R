library(tidyverse)
library(dplyr)
library(ggplot2)
library(explore)
library(tidyr)
library(caret)
setwd("C:/Users/MIIT/Desktop/DA")
coffee <- read.csv("Beverage.csv")
#view the dataframe
View(coffee)

###(A)-->Display the first few rows to understand the data's format.

#dimension 
dim(coffee)

#first 6 rows
head(coffee)

#last 6 rows 
tail(coffee)
###(B) --> Check the data types of each column (e.g., numeric, categorical).
#structure 
str(coffee)

###(C) -->Calculate basic summary statistics like mean, median, standard deviation, etc.
# Display basic summary statistics of the dataset
summary(coffee)

###(D) -->Handle Missing Data.
missing_values <- sum(is.na(coffee))
print(missing_values)

View(coffee)

# Replacing null with mean value(if needed)
mean_Caffeine <- mean(coffee$Caffeine, na.rm = TRUE)
mean_Caffeine
coffee$Caffeine[is.na(coffee$Caffeine)] <- mean_Caffeine
coffee$Caffeine

### --> (E) Data Cleaning and Transformation (Removing duplicates. 
#Encoding categorical variables, Scaling or normalizing numeric features,
#Handling outliers.)
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

#Encoding categorical variables
# Label encoding for categorical variables
coffee$Vitamin.A <- as.numeric(factor(coffee$Vitamin.A ))
coffee$Calcium <- as.numeric(factor(coffee$Calcium))
coffee$Iron <- as.numeric(factor(coffee$Iron))
View(coffee)

# Box plots to identify outliers
boxplot(coffee$Calories,coffee$Caffeine)

ggplot(coffee, aes(x = "", y = Calories)) + geom_boxplot() +
  labs(title = "Box Plot of Calories")
ggplot(coffee, aes(x = "", y = Caffeine)) + geom_boxplot() +
  labs(title = "Box Plot of Caffeine")
ggplot(coffee, aes(x = "", y = Total.Fat)) + geom_boxplot() +
  labs(title = "Box Plot of Total.Fat")
#Detect and Remove outliers
boxplot(coffee$Total.Fat,outline = TRUE)
boxplot(coffee$Calories,outline = TRUE)
boxplot(coffee$Caffeine,outline = TRUE)

###(F) -->Univariate Analysis (Create histograms, box plots, or bar charts
#to visualize the distribution of numeric and categorical variables. 
#Calculate and visualize summary statistics. Identify potential outliers.)

#Histogram of  numeric and categorical variable
#Histogram of Numerical variables
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
#Histogram of categorical variables
ggplot(coffee, aes(x = Calcium, fill = Calcium)) + geom_bar() +
  labs(title = "Distribution of Calcium")
ggplot(coffee, aes(x = Vitamin.A, fill = Vitamin.A)) + geom_bar() +
  labs(title = "Distribution of Vitamin A")
ggplot(coffee, aes(x = Iron,fill = Iron)) + geom_bar() +
  labs(title = "Distribution of Iron")
# Box plots of numeric variables to visualize distribution & potential outliers
ggplot(coffee, aes(x = 1, y = Calories)) + 
  geom_boxplot(fill = "lightblue", color = "green") +
  labs(title = "Box Plot of Calories ")
ggplot(coffee, aes(x = 1, y = Total.Fat)) + 
  geom_boxplot(fill = "lightblue", color = "green") +
  labs(title = "Box Plot of Total.Fat ")
ggplot(coffee, aes(x = 1, y = Caffeine)) + 
  geom_boxplot(fill = "lightblue", color = "green") +
  labs(title = "Box Plot of Caffeine ")

###(G) --> Bivariate and Multivariate Analysis (Create scatter plots, heat maps,
#or pair plots to visualize relationships between pairs of variables)

#scatter polts
ggplot(coffee, aes(x = Sugar, y = Calories)) +
  geom_point()

ggplot(coffee, aes(x = Cholesterol, y = Calories)) + geom_point() +
  labs(title = "Calories vs Cholesterol")

ggplot(coffee, aes(x = Calories, y = Carbohydrates)) + geom_point() +
  labs(title = "Calories vs Carbohydrates")

ggplot(coffee, aes(x = Calories, y = Sugar)) + geom_point() +
  labs(title = "Calories vs Sugar")
#Pair Plots
coffee_long <- pivot_longer(coffee,
                            c("Calories", "Total.Fat", "Caffeine"))

ggplot(coffee_long,
       aes(x = value,
           fill = name)) +
  geom_boxplot()

ggplot(coffee_long,
       aes(x = value)) +
  geom_histogram()+
  facet_wrap(name ~., scales = "free")

data<- dplyr::select(coffee,Calories,Cholesterol,Sugar,Carbohydrates)
pairs(data)
pairs(data,
      col = 'green',
      pch = 10,
      abels = c("var1", "var2", "var3"),
      main = "Pair Plot in R")
###(H)--> Compute correlation matrices to measure the strength
#and direction of relationships.
summarise(coffee, correlation = cor(x = Calories, y = Total.Fat ))
summarise(coffee, correlation = cor(x = Calories , y = Protein ))
summarise(coffee, correlation = cor(x = Calories , y = Carbohydrates ))
summarise(coffee, correlation = cor(x = Calories , y = Cholesterol ))
summarise(coffee, correlation = cor(x = Calories , y = Sugar))

###(I) -->Explore how different variables interact.
plot(coffee$Calories, coffee$Sugar, xlab="Calories", ylab="Sugar", main="Scatter Plot")

plot(coffee$Calories,coffee$Sugar,
     col = 'green',
     cex = 1.2,
     main = 'Scatterplot',
     xlab = 'Calories',
     ylab = 'Sugar',
     pch = 19)
# Bar plot to visualize
ggplot(coffee, aes(x = reorder(Beverage, -Calories), y = Calories)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pairs(coffee[, c("Calories", "Total.Fat", "Carbohydrates",
                 "Cholesterol", "Sugar", "Protein")])

ggplot(coffee, aes(x = Sugar, y = Calories, color = Beverage)) + geom_point()

boxplot(Total.Fat ~ Protein, data = coffee,
        xlab = "Protein",
        ylab = "Total Fat",
        main = "Total Fat Vs Protein",
        pch = 20,
        cex = 2,
        col = "lightseagreen",
        border = "green")

###(J)-->Use scatter plots, line charts, bar charts, 
#and other types of plots to represent patterns and trends.

# Scatter plot of 'Calories' vs. 'Sugar'
plot(coffee$Calories, coffee$Sugar, xlab="Calories",
     ylab="Sugar", main="Scatter Plot")
#Bar chart
ggplot(coffee, aes(x = reorder(Beverage, -Calories), y = Calories)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

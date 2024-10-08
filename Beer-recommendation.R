# Load required libraries
library(caret)
library(dplyr)

# Load the dataset
setwd("C:/Users/MIIT/Desktop")
beer_data <- read.csv("Beer.csv")

View(beer_data)

summary(beer_data)
###(A) -->Integrate and preprocess the data to create a clean and
          #unified dataset for analysis. This may involve data cleansing,
          #transformation, and enrichment.
# Label encoding for categorical variables
beer_data$Name <- as.numeric(factor(beer_data$Name))
beer_data$Style <- as.numeric(factor(beer_data$Style))
beer_data$Brewery <- as.numeric(factor(beer_data$Brewery))

# Summary statistics of the dataset
summary(beer_data)
View(beer_data)
# Check for missing values
missing_values <- colSums(is.na(beer_data))
print("Missing Values:")
print(missing_values)

###(B) --> Begin by conducting descriptive analytics to understand
          #historical trends and patterns in the data. 

# Correlation matrix
correlation_matrix <- cor(beer_data[, 4:18])
print("Correlation Matrix:")
View(correlation_matrix)

# Pairwise scatterplots for selected attributes
selected_attributes <- c("review_aroma", "review_appearance", "review_palate", "review_taste")
pairs(beer_data[, selected_attributes], pch = 16, main = "Pairwise Scatterplots")

###(C) -->#Use tools like data visualization, summary statistics to 
          #gain insights.

# Histogram of the target variable (review_overall)
hist(beer_data$review_overall, main = "Histogram of review_overall")

# Boxplots for numerical attributes
boxplot(beer_data[, 4:18], main = "Boxplots of Numerical Attributes")

# Bar plots for categorical attributes (e.g., Style)
library(ggplot2)
ggplot(beer_data, aes(x = Style)) +
  geom_bar() +
  labs(title = "Distribution of Beer Styles")

# Scatterplot matrix for numerical attributes
library(GGally)
ggpairs(beer_data[, 4:18], title = "Scatterplot Matrix")

# Density plots for review attributes (review_aroma, review_appearance, etc.)
ggplot(beer_data, aes(x = review_aroma)) +
  geom_density(fill = "blue") +
  labs(title = "Density Plot of review_aroma")

# Pairwise scatterplots for selected attributes
selected_attributes <- c("review_aroma", "review_appearance", "review_palate", "review_taste")
pairs(beer_data[, selected_attributes], pch = 16, main = "Pairwise Scatterplots")

# Histogram of review_overall ratings for each beer style
ggplot(beer_data, aes(x = review_overall)) +
  geom_histogram(binwidth = 0.1, fill = "green") +
  facet_wrap(~Style) +
  labs(title = "Histogram of Ratings by Beer Style")

###(E) -->Normalize your data according to the problem.
beer_data[, 4:18] <- scale(beer_data[, 4:18])


###(F) -->Build predictive models to forecast future outcomes based on 
#historical data.
# Split the data into training and testing sets
set.seed(123)  # for reproducibility
trainIndex <- createDataPartition(beer_data$review_overall, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
train_data <- beer_data[trainIndex, ]
test_data <- beer_data[-trainIndex, ]

# Build a linear regression model
model <- lm(review_overall ~ ., data = train_data)

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Evaluate the model 
# Mean Absolute Error (MAE)
mae <- mean(abs(predictions - test_data$review_overall))
print(paste("Mean Absolute Error (MAE):", mae))

###(H) -->Build a recommender system for User Based Content Filtering and Item Based Content Filtering.
N <- 10
recommendations <- test_data %>%
  select(Name, review_overall) %>%
  arrange(desc(predictions)) %>%
  head(N)

print("Top Recommendations:")
print(recommendations)
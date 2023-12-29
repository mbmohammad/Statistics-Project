# Load the CSV file
my_data <- read.csv("CarPrice_Assignment.csv")

# Create the boxplot
boxplot(my_data[, c("carlength", "wheelbase", "horsepower")],
        main = "Boxplot of Three Columns",
        xlab = "Columns",
        ylab = "Values")

# Replace missing values with the mean of the column
my_data$carlength[is.na(my_data$carlength)] <- median(my_data$carlength, na.rm = TRUE)
my_data$wheelbase[is.na(my_data$carheight)] <- median(my_data$wheelbase, na.rm = TRUE)
my_data$horsepower[is.na(my_data$horsepower)] <- median(my_data$horsepower, na.rm = TRUE)
my_data$carwidth[is.na(my_data$carwidth)] <- median(my_data$carwidth, na.rm = TRUE)
my_data$carlength[is.na(my_data$carlength)] <- median(my_data$carlength, na.rm = TRUE)
my_data$curbweight[is.na(my_data$curbweight)] <- median(my_data$curbweight, na.rm = TRUE)
my_data$boreratio[is.na(my_data$boreratio)] <- median(my_data$boreratio, na.rm = TRUE)
my_data$stroke[is.na(my_data$stroke)] <- median(my_data$stroke, na.rm = TRUE)
my_data$compressionratio[is.na(my_data$compressionratio)] <- median(my_data$compressionratio, na.rm = TRUE)
my_data$peakrpm[is.na(my_data$peakrpm)] <- median(my_data$peakrpm, na.rm = TRUE)
my_data$price[is.na(my_data$price)] <- median(my_data$price, na.rm = TRUE)
my_data$curbweight[is.na(my_data$curbweight)] <- median(my_data$curbweight, na.rm = TRUE)
my_data$boreratio[is.na(my_data$boreratio)] <- median(my_data$boreratio, na.rm = TRUE)

# Calculate the mode function
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Replace empty cells with mode
my_data$carbody[my_data$carbody == ""] <- get_mode(my_data$carbody)
my_data$cylindernumber[my_data$cylindernumber == ""] <- get_mode(my_data$cylindernumber)


# Create the boxplot
boxplot(my_data[, c("carlength", "wheelbase", "horsepower")],
        main = "Boxplot of Three Columns",
        xlab = "Columns",
        ylab = "Values")


# Select only numeric columns
numeric_data <- my_data[, sapply(my_data, is.numeric)]
# Compute the correlation matrix
cor_matrix <- cor(numeric_data[, !colnames(numeric_data) %in% c("X", "car_ID")])
# Print the correlation matrix
cor_matrix
# Load the required package
library(corrplot)
# Define the title
plot_title <- "Correlation Matrix Plot"
# Create the correlation plot with a title
corrplot(cor_matrix, method = "color", addCoef.col = 'white', main = plot_title)


variable1 <- my_data$carlength
variable2 <- my_data$wheelbase
# Calculate the correlation coefficient and perform a test
cor_test <- cor.test(variable1, variable2)
# Extract and examine the relevant information from the correlation test result
cor_coef <- cor_test$estimate   # Pearson correlation coefficient
p_value <- cor_test$p.value    # p-value
# Print the correlation test result
print(cor_test)
cat("p_value:          ", p_value, "\n")
# Check if the p-value is below a specific significance level (e.g., 0.05)
if (p_value < 0.05) {
  print("Carlength and Wheelbase are significantly correlated.")
} else {
  print("There is no significant correlation between Carlength and Wheelbase.")
}

variable1 <- my_data$compressionratio
variable2 <- my_data$enginesize
# Calculate the correlation coefficient and perform a test
cor_test <- cor.test(variable1, variable2)
# Extract and examine the relevant information from the correlation test result
cor_coef <- cor_test$estimate   # Pearson correlation coefficient
p_value <- cor_test$p.value    # p-value
# Print the correlation test result
print(cor_test)
cat("p_value:          ", p_value, "\n")
# Check if the p-value is below a specific significance level (e.g., 0.05)
if (p_value < 0.05) {
  print("Enginesize and Compressionratio are significantly correlated.")
} else {
  print("There is no significant correlation between Enginesize and Compressionratio.")
}

variable <- my_data$carheight
ttest_result <- t.test(variable, mu = 50)
# Extract the p-value
p_value <- ttest_result$p.value
# Print the t-test result
print(ttest_result)
# Check if the p-value is below a specific significance level (e.g., 0.05)
if (p_value < 0.05) {
  print("The mean of Car Height is significantly different from 50.")
} else {
  print("The mean of Car Height is not significantly different from 50.")
}

variable <- my_data$carwidth
ttest_result <- t.test(variable, mu = 66)
# Extract the p-value
p_value <- ttest_result$p.value
# Print the t-test result
print(ttest_result)
# Check if the p-value is below a specific significance level (e.g., 0.05)
if (p_value < 0.05) {
  print("The mean of Car Width is significantly different from 66.")
} else {
  print("The mean of Car Width is not significantly different from 66.")
}



# Define dummy variables for categorical columns
my_data.dummies <- model.matrix(~fueltype + aspiration + doornumber + carbody + drivewheel + enginelocation + enginetype + cylindernumber + fuelsystem - 1, data=my_data)
# Combine the original and dummy variables
my_data.final <- cbind(my_data.dummies, my_data)
# View the first few rows of the data frame
head(my_data.final)
#numeric values only
numeric_data1 <- my_data.final[,sapply(my_data.final, is.numeric)]
# Compute the correlation matrix
cor_matrix <- cor(numeric_data1)
corrplot(cor_matrix, method = "circle")


# Load the caret package
library(caret)
# Set the random seed for reproducibility
set.seed(123)
# Split the data set into training and testing sets
training.index <- createDataPartition(numeric_data1$price, p = 0.7, list = FALSE)
training <- numeric_data1[training.index, ]
testing <- numeric_data1[-training.index, ]


# Fit a multiple linear regression model
model <- lm(price ~ ., data = training[, -1])
# View the summary of the model
summary(model)

# Calculate the predicted values for the training data
predicted <- predict(model, newdata = training[, -1])
# Calculate the residuals
residuals <- training$price - predicted

# Calculate the RSS
RSS <- sum(residuals^2)
# Calculate the TSS
TSS <- sum((training$price - mean(training$price))^2)
# Calculate the MSE
MSE <- mean(residuals^2)
# Calculate the R-squared
Rsquared <- 1 - (RSS / TSS)
# Calculate the adjusted R-squared
N <- nrow(training)
p <- ncol(training) - 1
adjRsquared <- 1 - (1-Rsquared^2)*(N-1)/(N-p-1)

# Print the results
cat("RSS:          ", RSS, "\n")
cat("TSS:          ", TSS, "\n")
cat("MSE:          ", MSE, "\n")
cat("R-squared:    ", Rsquared, "\n")
cat("adj. R-squared:", adjRsquared, "\n")

# Plot the coefficients per feature
feature_names <- colnames(training)[-which(colnames(training) == "price")]
plot(coef(model)[-1], type = "b", xlab = "Features", ylab = "Coefficients", main = "Coefficients per Feature")
axis(1, at = 1:length(feature_names), labels = feature_names, las = 2)





# Normalize the training set
preprocessParams <- preProcess(training, method = c("center", "scale"))
training_normalized <- predict(preprocessParams, training)
# Fit a multiple linear regression model
model <- lm(price ~ ., data = training_normalized[, -1])
# View the summary of the model
summary(model)
# Plot the coefficients per feature
feature_names <- colnames(training_normalized)[-which(colnames(training_normalized) == "price")]
plot(coef(model)[-1], type = "b", xlab = "Features", ylab = "Coefficients", main = "Coefficients of normalized data per Feature")
axis(1, at = 1:length(feature_names), labels = feature_names, las = 2)

# Evaluate the model on the testing data
predictions <- predict(model, newdata = testing[, -1])


# Calculate the residual sum of squares (RSS) on the test data
residuals <- testing$price - predictions
RSS <- sum(residuals^2)

# Calculate the total sum of squares (TSS) on the test data
TSS <- sum((testing$price - mean(testing$price))^2)

# Calculate the mean squared error (MSE) on the test data
MSE <- mean(residuals^2)

# Calculate the R-squared on the test data
Rsquared <- 1 - (RSS / TSS)

# Calculate the adjusted R-squared on the test data
N <- nrow(testing)
p <- ncol(testing) - 1
adjRsquared <- 1 - (1-Rsquared)*(N-1)/(N-p-1)

# Print the performance metrics
cat("RSS:          ", RSS, "\n")
cat("TSS:          ", TSS, "\n")
cat("MSE:          ", MSE, "\n")
cat("R-squared     ", Rsquared, "\n")
cat("adjRsquared   ", adjRsquared, "\n")








# Perform t-test and select features based on p-values
p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
significant_features <- names(p_values[p_values < 0.05 & names(p_values) != "(Intercept)"])
significant_features <- significant_features[-which(significant_features == "X")]
# Subset the training and testing data with significant features
training_new <- training[, c("price", significant_features)]
testing_new <- testing[, c("price", significant_features)]
# Fit a new model with the reduced feature set
reduced_model <- lm(price ~ ., data = training_new)
# View the summary of the reduced model
summary(reduced_model)


# Evaluate the model on the testing data
predictions <- predict(reduced_model, newdata = testing_new[, -1])


# Calculate the residual sum of squares (RSS) on the test data
residuals <- testing_new$price - predictions
RSS <- sum(residuals^2)

# Calculate the total sum of squares (TSS) on the test data
TSS <- sum((testing_new$price - mean(testing_new$price))^2)

# Calculate the mean squared error (MSE) on the test data
MSE <- mean(residuals^2)

# Calculate the R-squared on the test data
Rsquared <- 1 - (RSS / TSS)

# Calculate the adjusted R-squared on the test data
N <- nrow(testing_new)
p <- ncol(testing_new) - 1
adjRsquared <- 1 - (1-Rsquared)*(N-1)/(N-p-1)

# Print the performance metrics
cat("RSS:          ", RSS, "\n")
cat("TSS:          ", TSS, "\n")
cat("MSE:          ", MSE, "\n")
cat("R-squared     ", Rsquared, "\n")
cat("adjRsquared   ", adjRsquared, "\n")




# Fit the linear regression model
model <- lm(price ~ ., data = training)
anova_table = anova(model)
sorted_anova_table <- anova_table[order(anova_table$`F value`, decreasing = TRUE), ]
selected_features = rownames(sorted_anova_table)
selected_features <- selected_features[-which(selected_features == "X")]
first_10_elements <- selected_features[1:10]
# Select the 10 features from the training dataset


model <- lm(price ~ ., data = training[, c(first_10_elements, "price")])
summary(model)
selected_features <- first_10_elements









# Fit the linear regression model
model <- lm(price ~ ., data = training_new)
anova_table = anova(model)
sorted_anova_table <- anova_table[order(anova_table$`F value`, decreasing = TRUE), ]
selected_features = rownames(sorted_anova_table)
first_10_elements <- selected_features[1:10]
# Select the 10 features from the training dataset

model <- lm(price ~ ., data = training_new[, c(first_10_elements, "price")])
summary(model)
selected_features <- first_10_elements








# Step 1: Load necessary libraries
library(dplyr)
# Step 3: Create a new dataframe with only the selected features
training_selected <- training %>% select(all_of(selected_features))
training_selected$price <- training$price

# Step 4: Create a new column for the synergy of the selected features (you can define this based on your problem)
training_selected$synergy <- training_selected$carwidth * training_selected$enginetypeohcv
model <- lm(price ~ ., data = training_selected)
summary(model)
training_selected$synergy <- training_selected$enginesize * training_selected$curbweight
model <- lm(price ~ ., data = training_selected)
summary(model)
training_selected$synergy <- training_selected$enginelocationrear * training_selected$carbodyhatchback
model <- lm(price ~ ., data = training_selected)
summary(model)
training_selected$synergy <- training_selected$aspirationturbo * training_selected$carbodywagon
model <- lm(price ~ ., data = training_selected)
summary(model)
training_selected$synergy <- training_selected$stroke * training_selected$cylindernumbertwelve
model <- lm(price ~ ., data = training_selected)
summary(model)
training_selected$synergy <- training_selected$carwidth * training_selected$carbodyhatchback
model <- lm(price ~ ., data = training_selected)
summary(model)
training_selected$synergy <- training_selected$carwidth * training_selected$enginelocationrear
model <- lm(price ~ ., data = training_selected)
summary(model)
training_selected$synergy <- training_selected$carbodywagon * training_selected$enginetypeohcv
model <- lm(price ~ ., data = training_selected)
summary(model)

training_selected$synergy <- training$enginetypeohc * training$enginetypeohcv
model <- lm(price ~ ., data = training_selected)
summary(model)


training_selected$synergy <- training$horsepower * training$enginesize
model <- lm(price ~ ., data = training_selected)
summary(model)


training_selected$synergy <- training$horsepower * training$enginesize
training_selected$synergy2 <- training$enginetypeohc * training$horsepower
training_selected$synergy3 <- training_selected$carbodywagon * training_selected$enginetypeohcv
training_selected$synergy4 <- training_selected$carwidth * training_selected$enginetypeohc
training_selected$synergy5 <- training_selected$carwidth * training_selected$carbodyhatchback
training_selected$synergy6 <- training_selected$stroke * training$carbodyhardtop
training_selected$synergy7 <- training_selected$aspirationturbo * training_selected$carbodywagon
training_selected$synergy8 <- training_selected$enginetypeohc * training_selected$carbodyhatchback
training_selected$synergy9<- training_selected$enginesize * training_selected$curbweight
training_selected$synergy10 <- training_selected$carwidth * training$carbodyhardtop
model <- lm(price ~ ., data = training_selected)
summary(model)




library(e1071)  # For SVM modeling
library(caret)  # For data partitioning

set.seed(123)  # Set a seed for reproducibility
numeric_data1 <- numeric_data1[, -which(names(numeric_data1) == "cylindernumberthree")]
training.index <- createDataPartition(numeric_data1$price, p = 0.7, list = FALSE)
training <- numeric_data1[training.index, ]
testing <- numeric_data1[-training.index, ]
svm_model <- svm(price ~ ., data = training, kernel = "radial")
summary(svm_model)

# Calculate predictions
predictions <- predict(svm_model, newdata = testing)

# Calculate the residual sum of squares (RSS) on the test data
residuals <- testing$price - predictions
RSS <- sum(residuals^2)

# Calculate the total sum of squares (TSS) on the test data
TSS <- sum((testing$price - mean(testing$price))^2)

# Calculate the mean squared error (MSE) on the test data
MSE <- mean(residuals^2)

# Calculate the R-squared on the test data
Rsquared <- 1 - (RSS / TSS)

# Calculate the adjusted R-squared on the test data
N <- nrow(testing)
p <- ncol(testing) - 1
adjRsquared <- 1 - (1-Rsquared)*(N-1)/(N-p-1)

# Print the performance metrics
cat("RSS:          ", RSS, "\n")
cat("TSS:          ", TSS, "\n")
cat("MSE:          ", MSE, "\n")
cat("R-squared     ", Rsquared, "\n")
cat("adjRsquared   ", adjRsquared, "\n")


tolerance <- 2000  # Set the tolerance level

# Assuming 'predictions' contain the SVM model predictions and 'actual_labels' contains the true class labels
accurate_predictions <- abs(predictions - testing$price) <= tolerance
accuracy <- sum(accurate_predictions) / length(testing$price)
accuracy

install.packages("randomForest")  # install the randomForest package if not already installed
library(randomForest)
# Specify the predictor variables and the target variable
predictors <- names(training)[-which(names(training) == "price")]
target <- "price"
# Train the Random Forest model
rf_model <- randomForest(x = training[, predictors],
                         y = training[, target],
                         ntree = 100,  # number of trees in the forest
                         importance = TRUE)  # calculate variable importance
predictions <- predict(rf_model, newdata = testing[, predictors])

# Mean Absolute Error (MAE)
mae <- mean(abs(predictions - testing$price))
# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((predictions - testing$price)^2))
# R-squared (coefficient of determination)
r_squared <- 1 - sum((predictions - testing$price)^2) / sum((mean(training$price) - testing$price)^2)


cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", r_squared, "\n")




install.packages("e1071")
library(e1071)
# Specify the formula for the model
formula <- price ~ .
# Train the SVR model
svr_model <- svm(formula, data = training, kernel = "radial", epsilon = 0.5)
# Make predictions on testing data
predictions <- predict(svr_model, newdata = testing)
mae <- mean(abs(predictions - testing$price))
rmse <- sqrt(mean((predictions - testing$price)^2))
ssr <- sum((predictions - mean(testing$price))^2)
sst <- sum((testing$price - mean(testing$price))^2)
r2 <- 1 - (ssr / sst)
n <- nrow(testing)
p <- ncol(testing) - 1  # Exclude the target variable
adj_r2 <- 1 - ((1 - r2) * ((n - 1) / (n - p - 1)))
mae
rmse
r2
adj_r2



install.packages("caret")
install.packages("rpart")
library(caret)
library(rpart)
# Specify the formula for the model
formula <- price ~ .
# Train the decision tree model
dt_model <- rpart(formula, data = training)
# Predict on the testing data
predictions <- predict(dt_model, newdata = testing)
# Calculate the mean squared error (MSE) for the predictions
mse <- caret::RMSE(predictions, testing$price)
print(paste("Root Mean Squared Error (RMSE):", mse))
tolerance <- 2000  # Set the tolerance level
# Assuming 'predictions' contain the SVM model predictions and 'actual_labels' contains the true class labels
accurate_predictions <- abs(predictions - testing$price) <= tolerance
accuracy <- sum(accurate_predictions) / length(testing$price)
accuracy












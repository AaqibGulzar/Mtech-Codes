# Step 1: Load the necessary libraries
library(ggplot2)
library(dplyr)

# Step 2: Assume you have a dataset named "cancer_data" with columns "tumor_size" and "diagnosis" (0: benign, 1: malignant)

# Step 3: Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(cancer_data), 0.7 * nrow(cancer_data))
train_data <- cancer_data[train_indices, ]
test_data <- cancer_data[-train_indices, ]

# Step 4: Fit the logistic regression model
model <- glm(diagnosis ~ tumor_size, data = train_data, family = binomial)

# Step 5: Predict probabilities on the testing set
test_data$predicted_prob <- predict(model, newdata = test_data, type = "response")

# Step 6: Classify based on a threshold (e.g., 0.5)
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)

# Step 7: Evaluate model performance
accuracy <- sum(test_data$predicted_class == test_data$diagnosis) / nrow(test_data)
print(paste("Accuracy:", accuracy))

# Step 8: Visualization (scatter plot with predicted classes)
ggplot(test_data, aes(x = tumor_size, y = factor(diagnosis))) +
  geom_point(aes(color = factor(predicted_class))) +
  labs(x = "Tumor Size", y = "Diagnosis", color = "Predicted Class")

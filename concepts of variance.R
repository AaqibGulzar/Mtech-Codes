# Load the data
data(cars)

# Fit the model
model <- lm(dist ~ speed, data = cars)

# Print the R-squared value
summary(model)$r.squared


# Calculate the model error variance
summary(model)$sigma^2

# Visualize the model
plot(cars)
abline(model, col = "red")


total_variance <- var(cars$dist)
explained_variability <- summary(model)$r.squared * total_variance
model_error_variance <- total_variance - explained_variability
# model_error_variance is same as rss/49, so using n-1 only...

rss = model$residuals^2 %>% sum()
rss/48 # this is exactly equal to sigma squared, 48 is n-k-1

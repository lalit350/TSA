# QUESTION 1
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")

# Load required libraries
library(forecast)
library(ggplot2)

# Create the time series data
time <- 1:15
values <- c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240)
ts_data <- ts(values, start = 1, frequency = 1)

# a. Plot the ACF and PACF of the original time series
par(mfrow = c(1, 2))  # Set up the plotting area for ACF and PACF
acf(ts_data, main = "ACF of Original Time Series")
pacf(ts_data, main = "PACF of Original Time Series")

# b. Fit an ARMA(1,1) model to the data
arma_model <- Arima(ts_data, order = c(1, 0, 1))

# Print the model summary
print(summary(arma_model))

# c. Check the residual diagnostics of the fitted model
checkresiduals(arma_model)  # This will plot the residuals, ACF of residuals, and a histogram

# d. Forecast the next 10 values using the fitted model
forecasted_values <- forecast(arma_model, h = 10)

# Print the forecasted values
print(forecasted_values)

# e. Plot the original time series, the fitted values, and the forecasted values
plot(ts_data, main = "Original and Forecasted Values", xlab = "Time", ylab = "Values", col = "blue", lwd = 2)
lines(fitted(arma_model), col = "green", lwd = 2)  # Add fitted values from the model

# Add forecasted values as points for better visibility
points((length(ts_data) + 1):(length(ts_data) + 10), forecasted_values$mean, col = "red", pch = 16)  # Plot forecasted points
lines(forecasted_values$mean, col = "red", lwd = 2)  # Connect forecasted values with a line

legend("topleft", legend = c("Original", "Fitted", "Forecasted"), col = c("blue", "green", "red"), lwd = 2, pch = c(NA, NA, 16))

# Add grid for better visualization
grid()












# QUESTION 2
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")

# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)

# Create the time series data
sales_data <- c(500, 520, 540, 560, 580, 600, 620, 640, 660, 680, 700, 720)
ts_data <- ts(sales_data, frequency = 4)  # Frequency set to 4 for quarterly data

# a. Plot the ACF and PACF of the original time series
par(mfrow=c(1,2))  # Set up the plotting area
acf(ts_data, main="ACF of Original Time Series")
pacf(ts_data, main="PACF of Original Time Series")

# b. Fit an ARMA(1,1) model to the data
arma_model <- Arima(ts_data, order = c(1, 0, 1))  # ARMA(1,1) implies differencing=0

# c. Check the residual diagnostics of the fitted model
checkresiduals(arma_model)

# d. Forecast the next 12 values using the fitted model
forecasted_values <- forecast(arma_model, h = 12)

# Print the forecasted values
print(forecasted_values)

# e. Plot the original time series, the fitted values, and the forecasted values
plot(ts_data, main = "Original and Forecasted Values", xlab = "Time", ylab = "Values", col = "blue", lwd = 2)
lines(fitted(arma_model), col = "green", lwd = 2)  # Add fitted values from the model

# Add forecasted values as points for better visibility
points((length(ts_data) + 1):(length(ts_data) + 10), forecasted_values$mean, col = "red", pch = 16)  # Plot forecasted points
lines(forecasted_values$mean, col = "red", lwd = 2)  # Connect forecasted values with a line

legend("topleft", legend = c("Original", "Fitted", "Forecasted"), col = c("blue", "green", "red"), lwd = 2, pch = c(NA, NA, 16))

# Add grid for better visualization
grid()


# QUESTION 1

install.packages("forecast")

# Load necessary package
library(forecast)

# Input data: years and observations
year <- 1996:2019
observation <- c(150.3, 150.9, 151.4, 151.9, 152.5, 152.9, 153.2, 153.7, 153.6, 
                 153.5, 154.4, 154.9, 155.7, 156.3, 156.6, 156.7, 157, 157.3, 
                 157.8, 158.3, 158.6, 158.6, 159.1, 159.3)

# Convert observations to a time series object
ts_data <- ts(observation, start = 1996, end = 2019, frequency = 1)

# a) Simple Exponential Smoothing (SES) with alpha = 0.3
ses_model <- ses(ts_data, alpha = 0.3, initial = "simple")

# b) Holt’s Exponential Smoothing with alpha = 0.3 and beta = 0.2
holt_model <- holt(ts_data, alpha = 0.3, beta = 0.2, initial = "simple")

# c) Plotting the original series and both smoothed series for comparison
plot(ts_data, col = "black", lwd = 2, ylab = "Observation", 
     main = "Comparison of Original and Smoothed Series", xlab = "Year")

# Add smoothed lines to the plot
lines(ses_model$fitted, col = "blue", lwd = 2, lty = 2)   # SES Smoothed series
lines(holt_model$fitted, col = "red", lwd = 2, lty = 3)   # Holt Smoothed series

# Add a legend for better clarity
legend("topleft", legend = c("Original", "SES (α=0.3)", "Holt (α=0.3, β=0.2)"), 
       col = c("black", "blue", "red"), lty = c(1, 2, 3), lwd = 2)

# Conclusion: Interpretation of the results
cat("\nConclusion:\n")
cat("1. Simple Exponential Smoothing (SES) provides a smoothed version of the series, but may not fully capture trends.\n")
cat("2. Holt's method better captures both level and trend in the data.\n")
cat("3. From the plots, observe that Holt's method aligns more closely with the original series, especially towards the end.\n")








# QUESTION 2

install.packages("forecast")

# Load necessary package
library(forecast)

# Data: Year and CO2 Concentration
year <- 1991:2003
co2_concentration <- c(355.62, 356.36, 357.1, 358.86, 360.9, 
                       362.58, 363.84, 366.58, 368.3, 369.47, 
                       371.03, 373.61, 357.61)

# Part (a): Time Series Plot
plot(year, co2_concentration, type = "o", col = "blue", lwd = 2, pch = 19,
     xlab = "Year", ylab = "Average CO2 Concentration", 
     main = "Atmospheric CO2 Concentration at Mauna Loa (1991-2003)")
grid()  # Add gridlines for better visualization

# Part (b): 3-Year Moving Average Forecasting
# Manually compute the 3-year moving average
moving_avg <- rep(NA, length(co2_concentration))  # Initialize with NA
for (i in 3:length(co2_concentration)) {
  moving_avg[i] <- mean(co2_concentration[(i-2):i])
}

# Display Moving Averages
cat("3-Year Moving Average:\n", moving_avg, "\n")

# Forecasting the 2004 value using the last 3 years (2001-2003)
forecast_2004 <- mean(co2_concentration[(length(co2_concentration)-2):length(co2_concentration)])
cat("Forecasted CO2 concentration for 2004:", forecast_2004, "\n")









# QUESTION 3
  
install.packages("forecast")

# Load necessary libraries
library(forecast)

# Load the AirPassengers dataset
data("AirPassengers")

# Apply the Holt-Winters method with multiplicative seasonality
holt_winters_model <- HoltWinters(AirPassengers, seasonal = "multiplicative")

# Print the model summary
summary(holt_winters_model)

# Forecast the next 12 months
forecasted_values <- forecast(holt_winters_model, h = 12)

# Print the forecasted values
print(forecasted_values)

# Plot the original data along with the forecast
plot(forecasted_values, main = "Holt-Winters Forecast for AirPassengers Data",
     ylab = "Number of Passengers", xlab = "Year", col = "blue")


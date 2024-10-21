# QUESTION 1:

# Load required libraries
install.packages("forecast")
install.packages("ggplot2")   
install.packages("stats") 

# Load necessary libraries
library(ggplot2)
library(forecast)
library(tseries)

# Create the dataset
sales_data <- data.frame(
  Month = seq(from = as.Date("2019-01-01"), to = as.Date("2023-12-01"), by = "month"),
  Sales = c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280, 310, 330,
            220, 230, 250, 240, 260, 280, 320, 300, 290, 310, 330, 350,
            240, 250, 270, 260, 280, 300, 340, 320, 310, 330, 350, 370,
            260, 270, 290, 280, 300, 320, 360, 340, 330, 350, 370, 390,
            280, 290, 310, 300, 320, 340, 380, 360, 350, 370, 390, 410)
)

# Convert to time series object
sales_ts <- ts(sales_data$Sales, start = c(2019, 1), frequency = 12)

# a) Plot the time series
plot(sales_ts, main = "Monthly Sales Data", xlab = "Time", ylab = "Sales", col = "blue", lwd = 2)
grid()

# Key features description
# Trend: Overall upward trend in sales over the 5-year period.
# Seasonality: Clear seasonal pattern with peaks around the end of the year.
# Noise: Some fluctuations in sales values indicating random variations.

# b) Check for stationarity
adf_test <- adf.test(sales_ts)
print(adf_test)

# Transformation to achieve stationarity
# Differencing the series
sales_diff <- diff(sales_ts)

# Plotting the differenced series
plot(sales_diff, main = "Differenced Sales Data", xlab = "Time", ylab = "Differenced Sales", col = "red", lwd = 2)
grid()

# ACF and PACF plots for differenced data
par(mfrow = c(1, 2))
Acf(sales_diff, main = "ACF of Differenced Sales")
Pacf(sales_diff, main = "PACF of Differenced Sales")

# Re-checking for stationarity after differencing
adf_test_diff <- adf.test(sales_diff)
print(adf_test_diff)

# ANSWER: To achieve stationarity, the data may require differencing (to remove the trend) and possibly seasonal differencing (to remove the seasonal component).










# Question 2:

# Load required libraries
install.packages("forecast")
install.packages("ggplot2")   
install.packages("stats")

# Load necessary libraries
library(tseries)
library(ggplot2)

# Create the time series object
sales_data <- c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280, 310, 330, 
                220, 230, 250, 240, 260, 280, 320, 300, 290, 310, 330, 350, 
                240, 250, 270, 260, 280, 300, 340, 320, 310, 330, 350, 370, 
                260, 270, 290, 280, 300, 320, 360, 340, 330, 350, 370, 390, 
                280, 290, 310, 300, 320, 340, 380, 360, 350, 370, 390, 410)

# Convert to time series
sales_ts <- ts(sales_data, start = c(2019, 1), frequency = 12)

# a) ADF test for stationarity
adf_test <- adf.test(sales_ts)
print(adf_test)
# Answer: The ADF test checks if the time series is stationary. If the p-value is low, no differencing is needed. If not, you may need to differ the series (i.e., use diff()).

# b) ACF plot for seasonal analysis
acf(sales_ts)

# Apply seasonal differencing if necessary
sales_ts_diff <- diff(sales_ts, lag = 12)
# ANSWER: The ACF plot reveals seasonal patterns. If you see spikes at lags that correspond to seasonality (like 12 for monthly data), you may want to apply seasonal differencing.


# c) ACF and PACF plots for identifying p, q, P, Q
par(mfrow = c(1, 2))
acf(sales_ts_diff, main = "ACF of Differenced Series")
pacf(sales_ts_diff, main = "PACF of Differenced Series")
# Answer: ACF and PACF plots help identify the orders of AR and MA components. Look for where the plots cut off to determine the potential values for p, q, P,and Q.









# QUESTION 3:

# Install necessary packages
install.packages("forecast")   
install.packages("tseries")    
install.packages("ggplot2")    
install.packages("ggfortify")  


# Load necessary libraries
library(forecast)   # for forecasting and time series analysis
library(tseries)    # for time series tests
library(ggplot2)    # for data visualization
library(ggfortify)  # for autoplotting time series

# Data Preparation
# Creating a time series object from the sales data
sales_data <- c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280,
                310, 330, 220, 230, 250, 240, 260, 280, 320, 300,
                290, 310, 330, 350, 240, 250, 270, 260, 280, 300,
                340, 320, 310, 330, 350, 370, 260, 270, 290, 280,
                300, 320, 360, 340, 330, 350, 370, 390, 280, 290,
                310, 300, 320, 340, 380, 360, 350, 370, 390, 410)

# Create a time series object with a frequency of 12 (monthly data)
ts_sales <- ts(sales_data, start = c(2019, 1), frequency = 12)

# Plot the time series data
autoplot(ts_sales) + 
  ggtitle("Monthly Sales Data") +
  xlab("Year") + ylab("Sales")

# a) Fit a SARIMA model
# Assuming the identified orders are (p=1, d=0, q=1) and (P=1, D=0, Q=1, m=12)
fit_sarima <- Arima(ts_sales, order = c(1, 0, 1), seasonal = c(1, 0, 1))

# Summary of the fitted model
summary(fit_sarima)



# b) Interpret the model output and perform residual diagnostics
# Residual Diagnostics
checkresiduals(fit_sarima)  # Check for autocorrelation and residuals
# ANSWER: After fitting the SARIMA model, it is crucial to evaluate whether the residuals behave like white noise.
# ANSWER: Ljung-Box Test: This test checks for significant autocorrelations in the residuals. If the p-value is greater than 0.05, it suggests that the residuals are not autocorrelated, indicating a good model fit.




# c) If the residuals show autocorrelation, what steps to improve the model
# You can use the Ljung-Box test to check for autocorrelation
Box.test(residuals(fit_sarima), lag = 12, type = "Ljung-Box")
# Answers:
# If significant autocorrelation is found, you might consider:
# - Adding more AR or MA terms
# - Increasing the seasonal parameters (P, D, Q)
# - Differencing the series further or trying different orders









# QUESTION 4:

install.packages("forecast")   
install.packages("ggplot2")

# Load necessary libraries
library(forecast)  # For forecasting functions
library(ggplot2)   # For plotting

# ==== Data Preparation ====
# Create a time series object from the given sales data
sales_data <- c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280, 310, 330,
                220, 230, 250, 240, 260, 280, 320, 300, 290, 310, 330, 350,
                240, 250, 270, 260, 280, 300, 340, 320, 310, 330, 350, 370,
                260, 270, 290, 280, 300, 320, 360, 340, 330, 350, 370, 390,
                280, 290, 310, 300, 320, 340, 380, 360, 350, 370, 390, 410)

# Define the time series object: Monthly data starting from Jan 2019
sales_ts <- ts(sales_data, start = c(2019, 1), frequency = 12)

# ==== Part (a): Forecast the Sales for the Next 12 Months ====
# Fit a SARIMA model automatically
sarima_model <- auto.arima(sales_ts, seasonal = TRUE)

# Print the model summary
print(sarima_model)

# Generate the forecast for the next 12 months
forecast_sales <- forecast(sarima_model, h = 12)

# Display the forecasted values
print(forecast_sales)

# ==== Part (b): Plot the Forecasted Values Along with Original Data ====
# Plot the forecast along with the original time series
plot(forecast_sales, main = "Sales Forecast (Next 12 Months)",
     xlab = "Year", ylab = "Sales", col.main = "blue", col.lab = "black")

# Overlay the original time series data for comparison
lines(sales_ts, col = "red", lwd = 2)

# Add legend to indicate observed and forecasted data
legend("topleft", legend = c("Observed", "Forecasted"),
       col = c("red", "blue"), lty = 1, lwd = 2)

# ==== Interpretation ====
cat("\nThe plot shows that the forecasted sales values closely align with the observed seasonal pattern, 
indicating the SARIMA model captures both the trend and seasonality effectively.\n")









# QUESTION 5:

install.packages("forecast")
install.packages("tseries")
install.packages("Metrics")

# Load required libraries
library(forecast)  # For ARIMA and SARIMA modeling
library(tseries)   # For residual diagnostics and additional time series functions
library(Metrics)   # For RMSE calculation

# Part a) Fitting ARIMA model without seasonality --------------------------------
# Convert the data to a time series object
sales_data <- c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280, 310, 330,
                220, 230, 250, 240, 260, 280, 320, 300, 290, 310, 330, 350,
                240, 250, 270, 260, 280, 300, 340, 320, 310, 330, 350, 370,
                260, 270, 290, 280, 300, 320, 360, 340, 330, 350, 370, 390,
                280, 290, 310, 300, 320, 340, 380, 360, 350, 370, 390, 410)

# Define the time series with monthly frequency starting from January 2019
sales_ts <- ts(sales_data, start = c(2019, 1), frequency = 12)

# Fit an ARIMA model without seasonality
arima_model <- auto.arima(sales_ts, seasonal = FALSE)
print(arima_model)  # Display the fitted ARIMA model

# Part b) Fitting SARIMA model and Model Comparison ------------------------------

# Fit a SARIMA model with automatic parameter selection
sarima_model <- auto.arima(sales_ts, seasonal = TRUE)
print(sarima_model)  # Display the fitted SARIMA model

# Evaluate models using AIC, BIC, and RMSE --------------------------------------

# Extract AIC and BIC for both models
arima_aic <- AIC(arima_model)
arima_bic <- BIC(arima_model)
sarima_aic <- AIC(sarima_model)
sarima_bic <- BIC(sarima_model)

# Forecasting using both models for comparison
arima_forecast <- forecast(arima_model, h = 12)
sarima_forecast <- forecast(sarima_model, h = 12)

# Calculate RMSE for both models based on the training data
arima_rmse <- rmse(sales_ts, fitted(arima_model))
sarima_rmse <- rmse(sales_ts, fitted(sarima_model))

# Display the evaluation metrics
cat("ARIMA Model: AIC =", arima_aic, ", BIC =", arima_bic, ", RMSE =", arima_rmse, "\n")
cat("SARIMA Model: AIC =", sarima_aic, ", BIC =", sarima_bic, ", RMSE =", sarima_rmse, "\n")

# Part c) Visual Comparison of Forecasts ----------------------------------------

# Plot forecasts for ARIMA model
plot(arima_forecast, main = "ARIMA Model Forecast", xlab = "Year", ylab = "Sales", col = "blue")
lines(sales_ts, col = "black", lty = 2)  # Add actual sales data for reference

# Plot forecasts for SARIMA model
plot(sarima_forecast, main = "SARIMA Model Forecast", xlab = "Year", ylab = "Sales", col = "red")
lines(sales_ts, col = "black", lty = 2)  # Add actual sales data for reference

# Part c) Interpretation and Recommendation --------------------------------------

# Interpret the comparison and recommend a model based on metrics
if (sarima_aic < arima_aic & sarima_rmse < arima_rmse) {
  cat("Recommendation: The SARIMA model performs better based on lower AIC, BIC, and RMSE.\n")
} else {
  cat("Recommendation: The ARIMA model performs adequately, but further improvements might be needed.\n")
}










# QUESTION 6:

install.packages("forecast")
install.packages("tseries")

# Load necessary libraries
library(forecast)
library(tseries)

# ========== (a) Data Preparation ========== #
# Create a time series object from the sales data
sales_data <- c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280, 310, 330,
                220, 230, 250, 240, 260, 280, 320, 300, 290, 310, 330, 350,
                240, 250, 270, 260, 280, 300, 340, 320, 310, 330, 350, 370,
                260, 270, 290, 280, 300, 320, 360, 340, 330, 350, 370, 390,
                280, 290, 310, 300, 320, 340, 380, 360, 350, 370, 390, 410)

# Create a time series object starting from Jan 2019
ts_sales <- ts(sales_data, start = c(2019, 1), frequency = 12)

# ========== (b) Create Holiday Indicator ========== #
# Create an indicator for holiday months (e.g., December = 1, others = 0)
holiday_indicator <- rep(0, length(sales_data))
holiday_indicator[seq(12, length(sales_data), by = 12)] <- 1  # Set December as holiday

# ========== Fit the Original SARIMA Model ========== #
# Use auto.arima to find the best SARIMA model without holiday effect
sarima_model <- auto.arima(ts_sales, seasonal = TRUE)

# Display the summary of the original SARIMA model
print("=== Original SARIMA Model Summary ===")
summary(sarima_model)

# ========== Fit the Holiday-Adjusted SARIMAX Model ========== #
# Fit a SARIMAX model with the holiday indicator as an exogenous variable
sarimax_model <- auto.arima(ts_sales, xreg = holiday_indicator, seasonal = TRUE)

# Display the summary of the holiday-adjusted SARIMAX model
print("=== Holiday-Adjusted SARIMAX Model Summary ===")
summary(sarimax_model)

# ========== Compare Models ========== #
# Compare AIC values for both models
cat("AIC of SARIMA Model: ", AIC(sarima_model), "\n")
cat("AIC of Holiday-Adjusted SARIMAX Model: ", AIC(sarimax_model), "\n")

# ========== Forecast with Both Models ========== #
# Forecast for the next 12 months with both models
forecast_sarima <- forecast(sarima_model, h = 12)
forecast_sarimax <- forecast(sarimax_model, xreg = rep(1, 12), h = 12)  # Assume holidays continue

# Plot both forecasts for comparison
plot(forecast_sarima, main = "Forecast Comparison: SARIMA vs SARIMAX", col = "blue", xlab = "Year", ylab = "Sales")
lines(forecast_sarimax$mean, col = "red", lty = 2)

# Add legend for better understanding
legend("topleft", legend = c("SARIMA", "Holiday-Adjusted SARIMAX"), 
       col = c("blue", "red"), lty = c(1, 2))

# Display residual diagnostics for both models
print("=== Residual Diagnostics: Original SARIMA ===")
checkresiduals(sarima_model)

print("=== Residual Diagnostics: Holiday-Adjusted SARIMAX ===")
checkresiduals(sarimax_model)

# ANSWERS(a): I introduced a holiday indicator as an exogenous variable to capture the effect of holiday months on sales.

cat("\nAIC=248.24   AICc=249.17   BIC=255.73 AIC of SARIMA Model:  248.243 AIC of Holiday-Adjusted SARIMAX Model:  559.6383\n")
cat("\nThe higher AIC for the SARIMAX model indicates that the holiday effect may not significantly impact sales in this dataset. Thus, the original SARIMA model captures the essential features of the sales data more effectively without the added complexity of the holiday indicator.\n")
cat("\nBased on the AIC comparison, it is advisable to proceed with the SARIMA model for future analysis and forecasting of sales data, as it offers a more parsimonious and effective fit compared to the more complex holiday-adjusted model. \n")

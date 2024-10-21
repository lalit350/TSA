# question 1:

# Load required libraries
install.packages("forecast")
install.packages("ggplot2")   
install.packages("stats")       

# Load necessary libraries
library(stats)  # For ARIMA and statistical functions
library(ggplot2)  # For plotting
library(forecast)  # For forecasting and ACF/PACF functions

# Set seed for reproducibility
set.seed(123)

# Part a
# (i) Simulate 100 observations from AR(1) process
n <- 100
phi <- 0.5  # AR(1) parameter
epsilon <- rnorm(n, mean = 0, sd = 1)  # White noise
X <- numeric(n)  # Initialize the series
X[1] <- epsilon[1]  # First observation

for (t in 2:n) {
  X[t] <- phi * X[t - 1] + epsilon[t]
}

# (ii) Plot the time series
plot.ts(X, main = "AR(1) Process Time Series", ylab = "X_t", xlab = "Time", col = "blue")

# (iii) Estimate the AR(1) parameter using ACF
ar_model <- arima(X, order = c(1, 0, 0))  # Fit AR(1) model
phi_est <- ar_model$coef[1]  # Extract estimated phi
cat("Estimated AR(1) parameter (phi):", phi_est, "\n")

# Part b
# (i) Plot ACF and PACF of the series
par(mfrow = c(1, 2))  # Set up the plotting area
acf(X, main = "ACF of AR(1) Process")
pacf(X, main = "PACF of AR(1) Process")
par(mfrow = c(1, 1))  # Reset to single plot

# (ii) Discuss the behaviour of ACF and PACF
cat("Behavior of ACF and PACF for AR(1) Process:\n")
cat("ACF: Decays exponentially, indicating a dependency on past values.\n")
cat("PACF: Cuts off after lag 1, confirming the AR(1) nature of the process.\n")

# Part c
# (i) Fit AR(1) or AR(2) model to the data
ar2_model <- arima(X, order = c(2, 0, 0))  # Fit AR(2) model
cat("AR(2) Model Coefficients:\n")
print(ar2_model$coef)

# (ii) Forecast the next 10 observations and plot the forecast
forecasted_values <- forecast(ar2_model, h = 10)
plot(forecasted_values, main = "Forecast for AR(2) Model", ylab = "X_t", xlab = "Time")









# QUESTION 2:

# Load required libraries
install.packages("forecast")
install.packages("ggplot2")   
install.packages("stats")       

# Load necessary libraries
library(stats)
library(ggplot2)
library(forecast)

# Set seed for reproducibility
set.seed(123)

# a) (i) Simulate 100 observations from the MA(1) process
n <- 100
theta <- 0.5
epsilon <- rnorm(n, mean = 0, sd = 1)  # White noise
X <- numeric(n)
for (t in 2:n) {
  X[t] <- epsilon[t] + theta * epsilon[t - 1]
}
X[1] <- epsilon[1]  # First observation

# a) (ii) Plot the time series
time_series_data <- data.frame(Time = 1:n, Value = X)
ggplot(time_series_data, aes(x = Time, y = Value)) +
  geom_line() +
  labs(title = "MA(1) Process Time Series", x = "Time", y = "Value") +
  theme_minimal()

# a) (iii) Estimate the MA(1) parameter θ using maximum likelihood estimation
ma1_model <- Arima(X, order = c(0, 0, 1))  # Fit MA(1) model
theta_est <- ma1_model$coef[1]  # Extract the estimated theta
cat("Estimated MA(1) parameter θ:", theta_est, "\n")

# b) (i) Plot ACF and PACF of the series
par(mfrow = c(1, 2))  # Set up plotting area
acf(X, main = "ACF of MA(1) Process")
pacf(X, main = "PACF of MA(1) Process")
par(mfrow = c(1, 1))  # Reset plotting area

# b) (ii) Discuss the behavior of ACF and PACF for MA(1) process
cat("The ACF of an MA(1) process cuts off after lag 1, indicating the direct influence of the first lag.\n")
cat("The PACF of an MA(1) process decays gradually, reflecting the autoregressive structure of the model.\n")

# c) (i) Fit MA(1) or MA(2) model to the data
ma2_model <- Arima(X, order = c(0, 0, 2))  # Fit MA(2) model
summary(ma2_model)  # Summary of the fitted MA(2) model

# c) (ii) Forecast the next 10 observations and plot the forecast
forecast_horizon <- 10
forecasts <- forecast(ma2_model, h = forecast_horizon)
autoplot(forecasts) +
  labs(title = "Forecast for the Next 10 Observations", x = "Time", y = "Forecasted Value") +
  theme_minimal()


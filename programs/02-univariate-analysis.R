# =============================================================================#
# Macroeconometrics 
# Analysis
# =============================================================================#

# --------------------- I - Preliminary ------------------------------------
# Clear environment
rm(list=ls())

# 
uk_data <- read_csv("work-data/data-uk.csv", show_col_types = FALSE)
uk_data$date <- as.Date(as.yearqtr(uk_data$date, format = "%Y Q%q"))
uk_data$date <- as.yearqtr(uk_data$date, format = "Q%q %Y")

# --------------------- II - Plots ------------------------------------
ggplot(uk_data, aes(x = date, y = gdp)) +
  geom_line(color = "#2c7bb6", linewidth = 0.8) +
  scale_x_yearqtr(
    name = "Year",
    format = "Q%q %Y",
    breaks = seq(min(uk_data$date), max(uk_data$date), by = 3), # Every 2 years
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "GDP (in millions of GBP)",
    labels = scales::comma_format(scale = 1e-6), # Convert to millions, add commas
    expand = c(0, 0)
  ) +
  labs(
    title = "UK GDP Over Time (1960–2023)",
    caption = "Source: UK Statistical Office."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 10)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10)),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

#
ggplot(uk_data, aes(x = date, y = balance_payments)) +
  geom_line(color = "#2c7bb6", linewidth = 0.8) +
  scale_x_yearqtr(
    name = "Year",
    format = "Q%q %Y",
    breaks = seq(min(uk_data$date), max(uk_data$date), by = 3), # Every 2 years
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Balance of Payments (in millions of GBP)"
  ) +
  labs(
    title = "UK Balance of Payments Over Time (1960–2024)",
    caption = "Source: UK Statistical Office."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 10)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10)),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

#
ggplot(uk_data, aes(x = date, y = exchange_rate)) +
  geom_line(color = "#2c7bb6", linewidth = 0.8) +
  scale_x_yearqtr(
    name = "Year",
    format = "Q%q %Y",
    breaks = seq(min(uk_data$date), max(uk_data$date), by = 3), 
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Average Sterling exchange rate"
  ) +
  labs(
    title = "UK Exchange Rate Over Time (1997–2024)",
    caption = "Source: UK Statistical Office."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 10)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10)),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


# time series
uk_data_ts <- ts(uk_data[,-1], start(1955), frequency = 4)
uk_data_ts <- na.omit(uk_data_ts)
par(mfrow=c(3,1), mar=c(4, 4, 2, 1)) # Set up plot layout
plot(uk_data_ts[, "gdp"], main = "UK GDP (Level)", ylab = "Value", xlab="Time")
plot(uk_data_ts[, "balance_payments"], main = "UK Trade Balance (Level)", ylab = "Value", xlab="Time")
plot(uk_data_ts[, "exchange_rate"], main = "UK Exchange Rate (Level)", ylab = "Value", xlab="Time")
par(mfrow=c(1,1)) # Reset plot layout
 
#--------------- III - Unit Root and Stationarity Tests ----------------------

# we write down a function to perform tests
perform_tests <- function(series, series_name) {
  cat("\n--- Testing:", series_name, "---\n")
  # Augmented Dickey-Fuller Test (Unit Root; H0: unit root exists)
  adf_result <- adf.test(series, alternative = "stationary")
  cat("ADF Test (H0: Unit Root):\n")
  print(adf_result)
  
  # Kwiatkowski-Phillips-Schmidt-Shin Test (Stationarity; H0: series is stationary)
  kpss_result <- kpss.test(series)
  cat("\nKPSS Test (H0: Stationarity):\n")
  print(kpss_result)
  
  # Comment: Based on p-values, decide if the series appears stationary or has a unit root.
  # If non-stationary, consider differencing (e.g., diff(series)) and re-test.
}

perform_tests(uk_data_ts[, "gdp"], "GDP")
perform_tests(uk_data_ts[, "balance_payments"], "Trade Balance")
perform_tests(uk_data_ts[, "exchange_rate"], "Exchange Rate")

# take first-differences
gdp_diff <- diff(uk_data_ts[, "gdp"])
balance_payments_diff <- diff(uk_data_ts[, "balance_payments"])
exchange_rate_diff <- diff(uk_data_ts[, "exchange_rate"]) 
# I think it's not good to differentiate rates

perform_tests(na.omit(gdp_diff), "Differenced GDP")
perform_tests(na.omit(balance_payments_diff), "Differenced Trade Balance")
perform_tests(na.omit(exchange_rate_diff), "Differenced Exchange Rate")

#------------------ IV - Model Estimation  -------------------------------------
cat("\n1.3 ARMA Model Identification & Estimation...\n")

# We write down a function to select and estimate the best model
identify_estimate_arma <- function(series, series_name) {
  cat("\n--- Analyzing:", series_name, "---\n")
  
  target_series <- series 
  
  cat("Plotting ACF and PACF...\n")
  par(mfrow=c(1,2))
  Acf(target_series, main = paste("ACF for", series_name))
  Pacf(target_series, main = paste("PACF for", series_name))
  par(mfrow=c(1,1))
  cat("Use ACF/PACF plots to suggest potential AR(p), MA(q), or ARMA(p,q) orders.\n")
  
  cat("\nEstimating models using auto.arima (AICc selection)...\n")
  # auto.arima automatically selects the best ARMA model based on information criteria (AICc by default)
  best_model <- auto.arima(target_series, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  cat("Best model selected by auto.arima:\n")
  print(summary(best_model))
  cat("Information Criteria (AICc used for selection):\n")
  print(best_model$aicc) # Can also check $aic, $bic
  
  cat("\nChecking residuals of the selected model...\n")
  checkresiduals(best_model)
  # Comment on the ACF/PACF plots and the model selected by auto.arima.
  # Does the selected model make sense given the ACF/PACF? Are the residuals white noise?
  return(best_model)
}

gdp_model <- identify_estimate_arma(uk_data_ts[, "gdp"], "GDP")
balance_payments_model <- identify_estimate_arma(uk_data_ts[, "balance_payments"], "Trade Balance")
exchange_rate_model <- identify_estimate_arma(uk_data_ts[, "exchange_rate"], "Exchange Rate")

#---------------------- V - Forecast  ------------------------------------------

cat("\n1.4 Forecasting (Example: GDP)...\n")
# Define forecast horizon 
h <- 8

# Generate forecasts from the selected model
gdp_forecast <- forecast(gdp_model, h)
exchange_rate_forecast <- forecast(exchange_rate_model, h)
balance_payments_forecast <- forecast(balance_payments_model, h)

plot(gdp_forecast, main = "Forecasts for GDP from ARIMA(1,0,0) Model")
plot(exchange_rate_forecast, main = "Forecasts for Exchange Rate from ARIMA(2,0,0) Model")
plot(balance_payments_forecast, main = "Forecasts for Balance of Payments from ARIMA(1,0,4) Model")

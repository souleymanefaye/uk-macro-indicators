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
cat("\n1.1 Plotting time series in levels...\n")
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

cat("\n1.2 Unit Root & Stationarity Tests...\n")

# use function defined in `programs/00-master/0a-setup.R`
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

# there are structural breaks find a method to identify

# save stationary time series data


#------------------ IV - Model Estimation  -------------------------------------
cat("\n1.3 ARMA Model Identification & Estimation...\n")

# use function defined in 
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

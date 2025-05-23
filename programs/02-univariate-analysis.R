# =============================================================================#
# Macroeconometrics 
# Analysis
# =============================================================================#

# --------------------- I - Preliminary ------------------------------------

uk_data <- read_csv("work-data/data-uk.csv", show_col_types = FALSE)
uk_data$date <- as.Date(as.yearqtr(uk_data$date, format = "%Y Q%q"))
uk_data$date <- as.yearqtr(uk_data$date, format = "Q%q %Y")

# --------------------- II - Plots ------------------------------------
cat("\n1.1 Plotting time series in levels...\n")
gdp_plot <- ggplot(uk_data, aes(x = date, y = gdp)) +
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
    title = "UK GDP Over Time (1955–2023)",
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

ggsave("figures/gdp_plot.png", gdp_plot, width = 8, height = 5, dpi = 300)
#
bop_plot <- ggplot(uk_data, aes(x = date, y = balance_payments)) +
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
    title = "UK Balance of Payments Over Time (1955–2024)",
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

ggsave("figures/bop_plot.png", bop_plot, width = 8, height = 5, dpi = 300)

#
exchange_plot <- ggplot(uk_data, aes(x = date, y = exchange_rate)) +
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

ggsave("figures/exchange_plot.png", exchange_plot, width = 8, height = 5, dpi = 300)


# time series
uk_data_ts <- ts(uk_data[,-1], start=c(1955,1), end= c(2024,4), frequency = 4)
#uk_data_ts <- na.omit(uk_data_ts)

png("figures/uk_levels_base_plot.png", width = 1400, height = 900)

par(mfrow=c(2,2), mar=c(4, 4, 2, 1)) # Set up plot layout
plot(uk_data_ts[, "gdp"], main = "UK GDP (Level)", ylab = "Value", xlab="Time")
plot(uk_data_ts[, "balance_payments"], main = "UK Trade Balance (Level)", ylab = "Value", xlab="Time")
plot(uk_data_ts[, "exchange_rate"], main = "UK Exchange Rate (Level)", ylab = "Value", xlab="Time")
par(mfrow=c(1,1)) # Reset plot layout

dev.off()

# Take first differene here and add graph for it


# Filter and add graph

#--------------- III - Unit Root and Stationarity Tests ----------------------

cat("\n1.2 Unit Root & Stationarity Tests...\n")

# use function defined in `programs/00-master/0a-setup.R`
perform_tests(uk_data_ts[, "gdp"], "GDP")
perform_tests(uk_data_ts[, "balance_payments"], "Trade Balance")
perform_tests(uk_data_ts[, "exchange_rate"], "Exchange Rate")

# take first-differences
gdp_diff <- diff(uk_data_ts[, "gdp"])
bp_diff <- diff(uk_data_ts[, "balance_payments"])
erate_diff <- diff(uk_data_ts[, "exchange_rate"]) 

perform_tests(gdp_diff, "Differenced GDP")
perform_tests(bp_diff, "Differenced Trade Balance")
perform_tests(erate_diff, "Differenced Exchange Rate")
# The three series are stationary after first-differencing

# save stationary time series data
uk_data_stationary <- cbind(
  gdp_diff, bp_diff, erate_diff
  )

# extract time index
time_index <- time(uk_data_stationary) 

# Convert fractional time to year-quarter format
years <- floor(time_index)
quarters <- round((time_index - years) * 4) + 1

# Create a date string like "1990 Q1"
date_labels <- paste0(years, " Q", quarters)

uk_data_stationary <- as.data.frame(uk_data_stationary)

uk_data_stationary$Date <- date_labels

uk_data_stationary <- uk_data_stationary[,
  c("Date", names(uk_data_stationary)[1:(ncol(uk_data_stationary)-1)])
  ]

colnames(uk_data_stationary) <- c("Date","First-differenced GDP", 
                                  "First-differenced balance of payments", 
                                  "First-differenced exchange rate")

write.csv(uk_data_stationary, "work-data/uk_data_stationary.csv", row.names = FALSE)

#------------------ IV - Model Estimation  -------------------------------------
cat("\n1.3 ARMA Model Identification & Estimation...\n")

# use function defined in "0a-setup.R"


gdp_model <- identify_estimate_arma(gdp_diff, "GDP (first-differenced)")
bp_model <- identify_estimate_arma(bp_diff, "Trade Balance")
erate_model <- identify_estimate_arma(erate_diff, "Exchange Rate")

#---------------------- V - Forecast  ------------------------------------------

cat("\n1.4 Forecasting (Example: GDP)...\n")
# Define forecast horizon 
h <- 20

# Generate forecasts from the selected model
gdp_forecast <- forecast(gdp_model, h)
erate_forecast <- forecast(erate_model, h)
bp_forecast <- forecast(bp_model, h)

#plot(gdp_forecast, main = "Forecasts for GDP from ARIMA(1,0,0) Model")
#plot(exchange_rate_forecast, main = "Forecasts for Exchange Rate from ARIMA(2,0,0) Model")
#plot(balance_payments_forecast, main = "Forecasts for Balance of Payments from ARIMA(1,0,4) Model")

cat("In-sample performance for GDP:\n")

fitted_values_gdp <- fitted(gdp_forecast)

print(accuracy(gdp_forecast))

par(mfrow=c(1,1)) # Reset plot layout if needed
plot(gdp_forecast$x, col = "black", ylab = "GDP Value", 
     main = "GDP: Actual vs. In-sample Fitted Values",
     ylim = range(c(gdp_forecast$x, fitted_values_gdp), na.rm = TRUE))
lines(fitted_values_gdp, col = "blue", lty = 2)
legend("topleft", legend = c("Actual GDP", "In-sample Fitted (ARIMA)"), col = c("black", "blue"), lty = c(1,2))
cat("The plot above shows the original GDP series and the in-sample fitted values from the ARIMA model.\n")
cat("Accuracy metrics (like RMSE, MAE) for the in-sample period are printed above.\n")

cat("\nOut-of-sample forecasts for GDP:\n")
# The `plot(forecast_object)` shows both historical data and out-of-sample forecasts.
plot(gdp_forecast, main = paste("GDP Forecasts from", gdp_model$method))
abline(v = time(gdp_forecast$x)[length(gdp_forecast$x)], lty = 3, col = "gray") # Mark end of actuals
cat("The plot above shows the historical GDP data, the out-of-sample point forecasts for the next", h, "periods, and their prediction intervals.\n")
cat("Out-of-sample point forecasts:\n")
print(gdp_forecast$mean)


cat("\n--- Trade Balance Forecasting ---\n")

cat("In-sample performance for Trade Balance:\n")

fitted_values_bp <- fitted(bp_forecast)

print(accuracy(bp_forecast))

par(mfrow=c(1,1)) # Reset plot layout if needed
plot(bp_forecast$x, col = "black", ylab = "Trade Balance (first-difference)", 
     main = "Trade Balance: Actual vs. In-sample Fitted Values",
     ylim = range(c(bp_forecast$x, fitted_values_bp), na.rm = TRUE))
lines(fitted_values_bp, col = "blue", lty = 2)
legend("topleft", legend = c("Actual Trade Balance", "In-sample Fitted (ARIMA)"), col = c("black", "blue"), lty = c(1,2))
cat("The plot above shows the original Trade Balance series and the in-sample fitted values from the ARIMA model.\n")
cat("Accuracy metrics (like RMSE, MAE) for the in-sample period are printed above.\n")

cat("\nOut-of-sample forecasts for Trade Balance:\n")
# The `plot(forecast_object)` shows both historical data and out-of-sample forecasts.
plot(bp_forecast, main = paste("Trade balance Forecasts from", bp_model$method))
abline(v = time(bp_forecast$x)[length(bp_forecast$x)], lty = 3, col = "gray") # Mark end of actuals
cat("The plot above shows the historical Trade Balance data, the out-of-sample point forecasts for the next", h, "periods, and their prediction intervals.\n")
cat("Out-of-sample point forecasts:\n")
print(bp_forecast$mean)

# --- Exchange Rate Forecasting ---
cat("\n--- Exchange Rate Forecasting ---\n")

cat("In-sample performance for Exchange Rate:\n")
fitted_values_erate <- fitted(erate_forecast)

print(accuracy(erate_forecast))

par(mfrow=c(1,1)) # Reset plot layout if needed
plot(erate_forecast$x, col = "black", ylab = "Average Sterling Exchange Rate (First-Differenced)", 
     main = "Exchange Rate: Actual vs. In-sample Fitted Values",
     ylim = range(c(erate_forecast$x, fitted_values_erate), na.rm = TRUE))
lines(fitted_values_erate, col = "blue", lty = 2)
legend("topleft", legend = c("Actual Average Sterling Exchange Rate", "In-sample Fitted (ARIMA)"), col = c("black", "blue"), lty = c(1,2))
cat("The plot above shows the original GDP series and the in-sample fitted values from the ARIMA model.\n")
cat("Accuracy metrics (like RMSE, MAE) for the in-sample period are printed above.\n")

cat("\nOut-of-sample forecasts for exchange rate:\n")
plot(erate_forecast, main = paste("Exchange rate Forecasts from", erate_model$method))
abline(v = time(erate_forecast$x)[length(erate_forecast$x)], lty = 3, col = "gray") # Mark end of actuals
cat("The plot above shows the historical trade balance data, the out-of-sample point forecasts for the next", h, "periods, and their prediction intervals.\n")
cat("Out-of-sample point forecasts:\n")
print(erate_forecast$mean)
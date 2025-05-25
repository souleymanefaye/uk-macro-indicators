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

# define as time series
uk_data_ts <- ts(uk_data[,-1], start=c(1955,1), end= c(2024,4), frequency = 4)

# Apply HP filter
# For GDP
hp_gdp <- hpfilter(uk_data_ts[, "gdp"], freq = 1600, type = "lambda")
gdp_trend <- hp_gdp$trend

# For Balance of Payments
hp_balance <- hpfilter(uk_data_ts[, "balance_payments"], freq = 1600, type = "lambda")
balance_trend <- hp_balance$trend

# --- For Exchange Rate ---
hp_exchange <- hpfilter(na.omit(uk_data_ts[, "exchange_rate"]), freq = 1600, type = "lambda")
exchange_trend <- hp_exchange$trend

# --- Take first difference ---
gdp_diff <- diff(uk_data_ts[, "gdp"])
bp_diff <- diff(uk_data_ts[, "balance_payments"])
erate_diff <- diff(uk_data_ts[, "exchange_rate"]) 

# HP trend (from levels) in first differences
gdp_diff_trend <- diff(gdp_trend)
balance_diff_trend <- diff(balance_trend)
exchange_diff_trend <- diff(exchange_trend)

# --- Plotting ---
png("figures/uk_macro_levels_and_differences.png", width = 2600, height = 1400)

par(mfrow = c(2, 3),
    mar = c(5.5, 6.5, 4, 2),
    mgp = c(4, 1.5, 0),
    cex.lab = 2.8,        
    cex.axis = 2.5,      
    cex.main = 2.0,      
    oma = c(0, 0, 4, 0))

# Plot UK GDP with Trend
plot(uk_data_ts[, "gdp"], main = "", ylab = "Real GDP (Billions GBP)", xlab = "Year")
lines(gdp_trend, col = "red", lty = 2, lwd = 3) # Add dashed trend line
grid(col = "lightgray", lty = "dotted")
legend("topleft", 
       legend = c("Actual Data", paste0("HP Trend (\u03BB=", 1600, ")")),
       col = c("black", "red"), lty = c(1, 2), lwd = c(2, 3), cex = 3, bty = "n")


# Plot UK Trade Balance with Trend
plot(uk_data_ts[, "balance_payments"], main = "", ylab = "Trade Balance (Millions GBP)", xlab = "Year")
lines(balance_trend, col = "red", lty = 2, lwd = 3) # Add dashed trend line
grid(col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("Actual Data", paste0("HP Trend (\u03BB=", 1600, ")")),
       col = c("black", "red"), lty = c(1, 2), lwd = c(2, 3), cex = 3, bty = "n")


# Plot UK Exchange Rate with Trend
plot(uk_data_ts[, "exchange_rate"], main = "", ylab = "Exchange Rate (GBP per USD)", xlab = "Year")
lines(exchange_trend, col = "red", lty = 2, lwd = 3) # Add dashed trend line
grid(col = "lightgray", lty = "dotted")
legend("topleft",
       legend = c("Actual Data", paste0("HP Trend (\u03BB=", 1600, ")")),
       col = c("black", "red"), lty = c(1, 2), lwd = c(2, 3), cex = 3, bty = "n")


# Plot UK GDP (First Difference)
plot(gdp_diff, main = "", ylab = "Change in Real GDP (Billions GBP)", xlab = "Year", col = "black", bty = "l")
lines(gdp_diff_trend, col = "red", lty = 2, lwd = 3) # Plotting diff of level-trend
grid(col = "lightgray", lty = "dotted")
legend("topleft", legend = c("Actual Change", paste0("Change in HP Trend (\u03BB=", 1600, ")")), col = c("black", "red"), lty = c(1, 2), lwd = c(2, 3), cex = 3, bty = "n")

# Plot UK Trade Balance (First Difference)
plot(bp_diff, main = "", ylab = "Change in Trade Balance (Millions GBP)", xlab = "Year", col = "black", bty = "l")
lines(balance_diff_trend, col = "red", lty = 2, lwd = 3)
grid(col = "lightgray", lty = "dotted")
legend("topleft", # Adjust position based on data
       legend = c("Actual Change", paste0("Change in HP Trend (\u03BB=", 1600, ")")), col = c("black", "red"), lty = c(1, 2), lwd = c(2, 3), cex = 3, bty = "n")

# Plot UK Exchange Rate (First Difference)
plot(erate_diff, main = "", ylab = "Change in Exchange Rate (GBP per USD)", xlab = "Year", col = "black", bty = "l")
lines(exchange_diff_trend, col = "red", lty = 2, lwd = 3)
grid(col = "lightgray", lty = "dotted")
legend("topleft", # Adjust position based on data
       legend = c("Actual Change", paste0("Change in HP Trend (\u03BB=", 1600, ")")), col = c("black", "red"), lty = c(1, 2), lwd = c(2, 3), cex = 3, bty = "n")


par(mfrow = c(1, 1)) # Reset plot layout

dev.off()

#--------------- III - Unit Root and Stationarity Tests ----------------------

cat("\n1.2 Unit Root & Stationarity Tests...\n")

# use function defined in `programs/00-master/0a-setup.R`


perform_tests(uk_data_ts[, "gdp", drop = TRUE], "GDP")
perform_tests(uk_data_ts[, "balance_payments", drop = TRUE], "Trade Balance")
perform_tests(uk_data_ts[, "exchange_rate", drop = TRUE], "Exchange Rate")


perform_tests(gdp_diff, "GDP_diff", out_dir = "tables") 
perform_tests(bp_diff, "Trade Balance_diff", out_dir = "tables") 
perform_tests(erate_diff, "Exchange Rate_diff", out_dir = "tables") 

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
bp_model <- identify_estimate_arma(bp_diff, "Trade Balance (first-differenced)")
erate_model <- identify_estimate_arma(erate_diff, "Exchange Rate (first-differenced)")


#---------------------- V - Forecast  ------------------------------------------

cat("\n1.4 Forecasting (Example: GDP)...\n")

# Define reasonable forecast horizons
h_gdp <- 8     # 2 years (8 quarters) for structural policy impacts
h_bp <- 6       # 1.5 years (6 quarters) for trade cycle durations
h_erate <- 4    # 1 year (4 quarters) FX volatility risk low
# Generate forecasts from the selected model
gdp_forecast <- forecast(gdp_model, h = h_gdp)
erate_forecast <- forecast(erate_model, h= h_bp)
bp_forecast <- forecast(bp_model,  h=h_erate)

#plot(gdp_forecast, main = "Forecasts for GDP from ARIMA(1,0,0) Model")
#plot(exchange_rate_forecast, main = "Forecasts for Exchange Rate from ARIMA(2,0,0) Model")
#plot(balance_payments_forecast, main = "Forecasts for Balance of Payments from ARIMA(1,0,4) Model")

cat("In-sample performance for GDP:\n")

fitted_values_gdp <- fitted(gdp_forecast)

print(accuracy(gdp_forecast))
acc_mat <- accuracy(gdp_forecast)
acc_df  <- acc_mat |>
  as.data.frame() |>
  tibble::rownames_to_column(var = "set")
readr::write_csv(acc_df,
                 file.path(root, "tables", "GDP_accuracy_metrics.csv"))

# Custom theme settings for all plots
plot_theme <- function() {
  par(
    cex.lab = 1.5,    # Larger axis labels
    cex.axis = 1.3,   # Larger axis numbers
    cex.main = 1.7,   # Larger main titles
    mar = c(5, 5, 4, 2) + 0.1,  # Bottom, Left, Top, Right margins
    mgp = c(3, 1, 0)  # Axis label positions
  )
}

# --- GDP Forecasting ---
png(file.path(root, "figures/GDP_fitted_vs_actual.png"), 
    width = 1600, height = 900, res = 150)
plot_theme()
plot(gdp_forecast$x, col = "black", lwd = 2,
     ylab = "Change in Real GDP", xlab = "Year",
     main = "",
     ylim = range(c(gdp_forecast$x, fitted_values_gdp), na.rm = TRUE))
lines(fitted_values_gdp, col = "#1F77B4", lwd = 2, lty = 2)  # Blue color
grid(col = "gray80", lty = "dotted")
legend("topleft", legend = c("Actual Data", "In-sample Fitted (ARIMA)"),
       col = c("black", "#1F77B4"), lty = c(1, 2), lwd = 2,
       cex = 1.2, bg = "white")
dev.off()

png(file.path(root, "figures/GDP_forecast.png"), 
    width = 1600, height = 900, res = 150)
plot_theme()
plot(gdp_forecast, main = "",
     xlab = "Year", ylab = "Change in Real GDP",
     col = "black", fcol = "#D62728", flwd = 2,  # Red forecast line
     shaded = TRUE, shadecols = c("#F0F0F0", "#D9D9D9"),  # Gray intervals
     pi.col = "#D62728")  # Red prediction interval lines
abline(v = time(gdp_forecast$x)[length(gdp_forecast$x)], 
       lty = 2, col = "gray40", lwd = 2)
legend("topleft", 
       legend = c("Historical Data", "Point Forecast", "80% CI", "95% CI"),
       col = c("black", "#D62728", "#D62728", "#D62728"),
       lty = c(1, 1, 1, 1), lwd = c(2, 2, 8, 12),
       cex = 1.2, bg = "white")
dev.off()

# --- Trade Balance Forecasting ---
png(file.path(root, "figures/TradeBalance_fitted_vs_actual.png"), 
    width = 1600, height = 900, res = 150)
plot_theme()
plot(bp_forecast$x, col = "black", lwd = 2,
     ylab = "Change in Trade Balance", xlab = "Year",
     main = "",
     ylim = range(c(bp_forecast$x, fitted_values_bp), na.rm = TRUE))
lines(fitted_values_bp, col = "#1F77B4", lwd = 2, lty = 2)
grid(col = "gray80", lty = "dotted")
legend("topleft", legend = c("Actual Data", "In-sample Fitted (ARIMA)"),
       col = c("black", "#1F77B4"), lty = c(1, 2), lwd = 2,
       cex = 1.2, bg = "white")
dev.off()

png(file.path(root, "figures/TradeBalance_forecast.png"), 
    width = 1600, height = 900, res = 150)
plot_theme()
plot(bp_forecast, main = "",
     xlab = "Year", ylab = "Change in Trade Balance",
     col = "black", fcol = "#D62728", flwd = 2,
     shaded = TRUE, shadecols = c("#F0F0F0", "#D9D9D9"),
     pi.col = "#D62728")
abline(v = time(bp_forecast$x)[length(bp_forecast$x)], 
       lty = 2, col = "gray40", lwd = 2)
legend("topleft", 
       legend = c("Historical Data", "Point Forecast", "80% CI", "95% CI"),
       col = c("black", "#D62728", "#D62728", "#D62728"),
       lty = c(1, 1, 1, 1), lwd = c(2, 2, 8, 12),
       cex = 1.2, bg = "white")
dev.off()

# --- Exchange Rate Forecasting ---
png(file.path(root, "figures/ExchangeRate_fitted_vs_actual.png"), 
    width = 1600, height = 900, res = 150)
plot_theme()
plot(erate_forecast$x, col = "black", lwd = 2,
     ylab = "Change in Exchange Rate", xlab = "Year",
     main = "Exchange Rate: Actual vs. Fitted Values",
     ylim = range(c(erate_forecast$x, fitted_values_erate), na.rm = TRUE))
lines(fitted_values_erate, col = "#1F77B4", lwd = 2, lty = 2)
grid(col = "gray80", lty = "dotted")
legend("topleft", 
       legend = c("Actual Data", "In-sample Fitted (ARIMA)"),
       col = c("black", "#1F77B4"), lty = c(1, 2), lwd = 2,
       cex = 1.2, bg = "white")
dev.off()

png(file.path(root, "figures/ExchangeRate_forecast.png"), 
    width = 1600, height = 900, res = 150)
plot_theme()
plot(erate_forecast, main = "",
     xlab = "Year", ylab = "Change in Exchange Rate",
     col = "black", fcol = "#D62728", flwd = 2,
     shaded = TRUE, shadecols = c("#F0F0F0", "#D9D9D9"),
     pi.col = "#D62728")
abline(v = time(erate_forecast$x)[length(erate_forecast$x)], 
       lty = 2, col = "gray40", lwd = 2)
legend("topleft", 
       legend = c("Historical Data", "Point Forecast", "80% CI", "95% CI"),
       col = c("black", "#D62728", "#D62728", "#D62728"),
       lty = c(1, 1, 1, 1), lwd = c(2, 2, 8, 12),
       cex = 1.2, bg = "white")
dev.off()
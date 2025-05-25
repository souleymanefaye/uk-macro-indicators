# =============================================================================#
# Macroeconometrics 
# Analysis
# =============================================================================#


# --------------------- I - Preliminaries -----------------------------

# use stationary time series data
uk_data_stationary <- read_csv("work-data/uk_data_stationary.csv", 
                               show_col_types = FALSE)


var_data <- na.omit(
  ts(uk_data_stationary[,-1], start=c(1955,2), end= c(2024,4), frequency = 4))
# --------------------- II - Optimal Lag Selection -----------------------------

cat("\n2.1 Selecting Optimal VAR Lag Length...\n")
lag_selection <- VARselect(var_data, lag.max = 10, type = "const") 

cat("Lag length selection criteria:\n")
print(lag_selection$selection)
chosen_lag <- lag_selection$selection["AIC(n)"]

lags_df <- as.data.frame(t(lag_selection$selection))
write.csv(lags_df,
          file = file.path(root, "tables", "lag_selection.csv"),
          row.names = FALSE)


# --------------------- III - Model Estimation ---------------------------------


cat("\n2.2 Estimating VAR model...\n")

# Estimate the VAR model by OLS
var_ols <- VAR(var_data, p = chosen_lag, type = "const")
print(var_ols)

stargazer(var_ols$varresult,
          type = "latex", 
          title = "", # Remove title (often redundant in papers)
          style = "aer",
          column.labels = c("GDP", "Trade Balance", "FX"), # Simplified headers
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          covariate.labels = c(paste0("L", 1:7), # Generic lag labels (e.g., "L1" instead of "L1 GDP")
                               "Constant"), # Group constants at the end
          single.row = TRUE, 
          digits = 2, # Fewer decimals for cleaner look
          digits.extra = 0,
          omit.stat = c("ser", "f", "rsq", "adj.rsq"), # Remove R² and other stats
          notes = c("All variables are first-differenced.",
                    "Standard errors in parentheses; * p<0.1, ** p<0.05, *** p<0.01"), 
          notes.align = "l",
          header = FALSE,
          font.size = "small",
          label = "tab:var_results",
          model.names = FALSE, # Remove redundant equation labels
          order = c(1:7, 22), # Show only L1–L7 and Constant (adjust indices as needed)
          omit = c("L8", "L9")) # Drop unused lags (if applicable

cat("\nChecking VAR residuals...\n")

# Test for serial correlation (Portmanteau Test; H0: no serial correlation)
serial_test <- serial.test(var_ols, lags.pt = chosen_lag + 5, type = "PT.asymptotic")
cat("Residual Serial Correlation Test:\n")
print(serial_test)

# Test for heteroskedasticity (ARCH Test; H0: no ARCH effects)
arch_test <- arch.test(var_ols, lags.multi = chosen_lag + 1, multivariate.only = TRUE)
cat("\nResidual ARCH Test:\n")
print(arch_test)

# Test for normality (Jarque-Bera Test; H0: residuals are normally distributed)
normality_test <- normality.test(var_ols, multivariate.only = TRUE)
cat("\nResidual Normality Test:\n")
print(normality_test)

# Summarize the test statistics in a single .csv file
serial_df <- with(serial_test$serial, 
                  data.frame(
                    Test   = "Serial Correlation",
                    ChiSq  = statistic,
                    df     = parameter,
                    p.value= p.value
                  )
)

arch_df <- with(arch_test$arch.mul, 
                data.frame(
                  Test   = "ARCH",
                  ChiSq  = statistic,
                  df     = parameter,
                  p.value= p.value
                )
)

norm_df <- data.frame(
  Test    = "Normality",
  ChiSq   = normality_test$jb.mul$JB$statistic,
  df      = normality_test$jb.mul$JB$parameter,
  p.value = normality_test$jb.mul$JB$p.value,
  stringsAsFactors = FALSE
)

all_diag <- bind_rows(serial_df, arch_df, norm_df)
out_dir <- file.path(root, "tables")
write.csv(
  all_diag,
  file = file.path(out_dir, "VAR_diagnostics.csv"),
  row.names = FALSE
)
cat("VAR diagnostics saved to", file.path(out_dir, "VAR_diagnostics.csv"), "\n")

# --------------------- IV - Forecasts -----------------------------------------
cat("\n2.3 VAR Forecasting...\n")

# Get in-sample fitted values
var_fitted <- fitted(var_ols)
gdp_fitted <- var_fitted[,1]
# Forecast horizon (e.g., 8 periods)
h_var <- 8
var_forecast <- predict(var_ols, n.ahead = h_var, ci = 0.95) # 95% confidence intervals

cat("Plotting VAR forecasts (Example: GDP)...\n")

# Out of sample plots 
# For GDP (1st variable in var_data):
png(filename = file.path(root, "figures/VAR_forecast_GDP.png"), 
    width = 1600, height = 900, res = 150)
fanchart(var_forecast, names = "First.differenced.GDP", main = "")  # [1] = GDP
dev.off()

# For Trade Balance (2nd variable in var_data):
png(filename = file.path(root, "figures/VAR_forecast_TradeBalance.png"), 
    width = 1600, height = 900, res = 150)
fanchart(var_forecast, names = "First.differenced.balance.of.payments", main ="")  # [2] = Trade Balance
dev.off()

# For Exchange Rate (3rd variable in var_data):
png(filename = file.path(root, "figures/VAR_forecast_ExchangeRate.png"), 
    width = 1600, height = 900, res = 150)
fanchart(var_forecast, names = "First.differenced.exchange.rate", main = "")  # [3] = Exchange Rate
dev.off()

# ----------------------V - Cholesky Decomposition -----------------------------
cat("\n2.4 Cholesky Decomposition Ordering...\n")

chosen_order <-  c("First.differenced.exchange.rate", 
                      "First.differenced.GDP", "First.differenced.balance.of.payments") 


# --------------------- VI - Impulse Response Functions ------------------------
cat("\n2.5 Calculating and Plotting IRFs...\n")

irf_results_1 <- irf(var_ols, impulse = chosen_order, response = chosen_order,
                     n.ahead = 20, ortho = TRUE, boot = TRUE, ci = 0.95, runs = 100)

cat("Plotting IRFs (Order 1)...\n")
plot(irf_results_1, main = "")

# Gereon's plots
out_dir <- file.path(root, "figures", "IRF_plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# get the three impulse names
impulses <- names(irf_results_1$irf)

for (imp in impulses) {
  # build a safe filename
  fn <- file.path(out_dir, paste0("IRF_to_", imp, ".png"))
  
  png(fn, width = 900, height = 600, res = 120)
  # plot only this impulse (all of its response panels)
  plot(irf_results_1,
       impulse = imp,
       main    = "")
  dev.off()
  
  message("Wrote ", fn)
}

# ------------------- VII - Ordering of Variables ------------------------------
cat("\n2.6 Modifying Variable Ordering and Re-calculating IRFs...\n")

chosen_order2 <- c("First-differenced GDP", 
                   "First-differenced balance of payments", 
                   "First-differenced exchange rate") 
cat("New variable order for Cholesky:", paste(chosen_order2, collapse = ", "), "\n")

var_data_reordered <- var_data[, chosen_order2]
var_model_reordered <- VAR(var_data_reordered, p = chosen_lag, type = "const")

chosen_order2 <- c("First.differenced.GDP",  
                   "First.differenced.balance.of.payments",
                   "First.differenced.exchange.rate")

irf_results_2 <- irf(var_model_reordered, impulse = chosen_order2, response = chosen_order2,
                     n.ahead = 20, ortho = TRUE, boot = TRUE, ci = 0.95, runs = 100)

plot(irf_results_2,  main = "")



# Gereon's plots
out_dir2 <- file.path(root, "figures/IRF_plots2")
dir.create(out_dir2, recursive = TRUE, showWarnings = FALSE)

# get the three impulse names
impulses <- names(irf_results_2$irf)

for (imp in impulses) {
  # build a safe filename
  fn <- file.path(out_dir2, paste0("IRF_to_", imp, ".png"))
  
  png(fn, width = 900, height = 600, res = 120)
  # plot only this impulse (all of its response panels)
  plot(irf_results_2,
       impulse = imp,
       main    = "")
  dev.off()
  
  message("Wrote ", fn)
}

# ------------------- VII - Blanchard Quah long run restrictions ---------------




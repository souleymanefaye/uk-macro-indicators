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
var_model <- VAR(var_data, p = chosen_lag, type = "const")
print(summary(var_model))

cat("\nChecking VAR residuals...\n")

# Test for serial correlation (Portmanteau Test; H0: no serial correlation)
serial_test <- serial.test(var_model, lags.pt = chosen_lag + 5, type = "PT.asymptotic")
cat("Residual Serial Correlation Test:\n")
print(serial_test)

# Test for heteroskedasticity (ARCH Test; H0: no ARCH effects)
arch_test <- arch.test(var_model, lags.multi = chosen_lag + 1, multivariate.only = TRUE)
cat("\nResidual ARCH Test:\n")
print(arch_test)

# Test for normality (Jarque-Bera Test; H0: residuals are normally distributed)
normality_test <- normality.test(var_model, multivariate.only = TRUE)
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
# Forecast horizon (e.g., 8 periods)
h_var <- 8
var_forecast <- predict(var_model, n.ahead = h_var, ci = 0.95) # 95% confidence intervals

cat("Plotting VAR forecasts (Example: GDP)...\n")
#
png(filename = file.path(root, "figures/VAR_forecast1.png"), 
    width = 1600, height = 900, res = 150)
plot(var_forecast, names = colnames(var_data)[1])
dev.off()
#
png(filename = file.path(root, "figures/VAR_forecast2.png"), 
    width = 1600, height = 900, res = 150)
fanchart(var_forecast, names = colnames(var_data)[1])
dev.off()
# ----------------------V - Cholesky Decomposition -----------------------------
cat("\n2.4 Cholesky Decomposition Ordering...\n")

chosen_order <- c("First.differenced.GDP",  
                  "First.differenced.balance.of.payments",
                  "First.differenced.exchange.rate")

# --------------------- VI - Impulse Response Functions ------------------------
cat("\n2.5 Calculating and Plotting IRFs...\n")

irf_results_1 <- irf(var_model, impulse = chosen_order, response = chosen_order,
                     n.ahead = 20, ortho = TRUE, boot = TRUE, ci = 0.95, runs = 100)

cat("Plotting IRFs (Order 1)...\n")
plot(irf_results_1)

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
       main    = paste("IRF to", imp, "shock"))
  dev.off()
  
  message("Wrote ", fn)
}

# ------------------- VII - Ordering of Variables ------------------------------
cat("\n2.6 Modifying Variable Ordering and Re-calculating IRFs...\n")

chosen_order2 <- c("First-differenced exchange rate", 
                   "First-differenced balance of payments", 
                   "First-differenced GDP") 
cat("New variable order for Cholesky:", paste(chosen_order2, collapse = ", "), "\n")

var_data_reordered <- var_data[, chosen_order2]
var_model_reordered <- VAR(var_data_reordered, p = chosen_lag, type = "const")

chosen_order2 <- c("First.differenced.exchange.rate", 
                   "First.differenced.balance.of.payments", 
                   "First.differenced.GDP") 

irf_results_2 <- irf(var_model_reordered, impulse = chosen_order2, response = chosen_order2,
                     n.ahead = 20, ortho = TRUE, boot = TRUE, ci = 0.95, runs = 100)

plot(irf_results_2)



# Gereon's plots
out_dir <- file.path(root, "figures", "IRF_plots2")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# get the three impulse names
impulses <- names(irf_results_2$irf)

for (imp in impulses) {
  # build a safe filename
  fn <- file.path(out_dir, paste0("IRF_to_", imp, ".png"))
  
  png(fn, width = 900, height = 600, res = 120)
  # plot only this impulse (all of its response panels)
  plot(irf_results_2,
       impulse = imp,
       main    = paste("IRF to", imp, "shock"))
  dev.off()
  
  message("Wrote ", fn)
}





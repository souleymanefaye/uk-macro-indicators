# ==============================================================================
# Macroeconometrics 
# 
# ==============================================================================

# Clear environment
rm(list=ls())

# set the WD to a new folder
#root <- setwd("~/Documents/Projects/R/FG2025-uk-macro-indicators") # Souley's path
root <- setwd("C:/A Eigene Ordner/Studium/Paris School of Economics/M1/Courses/Econometrics 2b/Homework/uk-macro-indicators") # Gereon's path

# subdirectories
code <- file.path(root, "programs")
work <- file.path(root, "work-data")
raw <- file.path(root, "raw-data")
figures <- file.path(root, "figures")
tables <- file.path(root, "tables")


# Load libraries via pacman
if (!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman")

pacman::p_load(tsibble, patchwork, tidyverse, zoo, vars, urca, tseries, forecast, broom, readr, dplyr)


# libraries (manual method) 
#library(readr)
#library(lubridate)
#library(tsibble)
#library(patchwork)
#library(tidyverse) 
#library(zoo) # for quarter year type variables
#library(vars) # package for selecting the optimal lag-order
#library(urca) # package for testing unit roots
#library(ggplot2)
#library(tseries)   # For time series tests (adf.test, kpss.test)
#library(forecast)  # For ARMA modeling and forecasting (auto.arima, forecast, Acf, Pacf)

# functions
# we write down a function to perform tests
perform_tests <- function(series, series_name) {
  series <- na.omit(series) 
  cat("\n--- Testing:", series_name, "---\n")
  # Augmented Dickey-Fuller Test (Unit Root; H0: unit root exists)
  adf_result <- adf.test(series, alternative = "stationary")
  cat("Augmented Dickey-Fuller Test (H0: Unit Root (non-stationary)):\n")
  print(adf_result)
  
  # Kwiatkowski-Phillips-Schmidt-Shin Test (Stationarity; H0: series is stationary)
  kpss_result <- kpss.test(series)
  cat("\n Kwiatkowski-Phillips-Schmidt-Shin Test (H0: Stationarity):\n")
  print(kpss_result)
  
  # Elliott, Rothenberg and Stock Unit Root Test
  ers_result <- ur.ers(series, type = "DF-GLS")
  cat("\n Elliott, Rothenberg and Stock Test (H0: Unit Root (non-stationary)):\n")
  print(summary(ers_result))
  
  # Phillips-Perron Test
  pp_result <- pp.test(series)
  cat("\n Phillips-Perron Test (H0: Unit Root (non-stationary)):\n")
  print(pp_result)
  
  # Comment: Based on p-values, decide if the series appears stationary or has a unit root.
  # If non-stationary, take first difference and re-test.
}

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




tidy_test <- function(obj, test_name) {
  if (inherits(obj, "htest")) {                # ADF, KPSS, PP
    out <- glance(obj) |>
      transmute(statistic = statistic,
                p.value  = p.value,
                test     = test_name)
    
    # Some htests (ADF, KPSS) do not carry critical values.
    # Add NA columns now so the final table is rectangular.
    out$crit_1pct <- out$crit_5pct <- out$crit_10pct <- NA_real_
    
  } else if (inherits(obj, "ur.df") |
             inherits(obj, "ur.ers") |
             inherits(obj, "ur.pp")) {        # ERS, alternative urca tests
    sm <- summary(obj)
    out <- tibble(
      statistic = sm@teststat[1],             # the first column is test-stat
      crit_1pct = sm@cval[1, "1pct"],
      crit_5pct = sm@cval[1, "5pct"],
      crit_10pct= sm@cval[1, "10pct"],
      p.value   = NA_real_,                   # urca summaries give no p-value
      test      = test_name
    )
  } else {
    stop("Unknown test object")
  }
  out
}


# ==============================================================================
# Macroeconometrics 
# 
# ==============================================================================

# Clear environment
rm(list=ls())

# set the WD to a new folder
#root <- setwd("~/Documents/Projects/R/FG2025-uk-macro-indicators") # Souley's path
root <- setwd("C:/A Eigene Ordner/Studium/Paris School of Economics/M1/Courses/Econometrics 2b/Homework/uk-macro-indicators") # Gereon's path

# subdirectories
code <- file.path(root, "programs")
work <- file.path(root, "work-data")
raw <- file.path(root, "raw-data")
figures <- file.path(root, "figures")
tables <- file.path(root, "tables")


# Load libraries via pacman
if (!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman")

pacman::p_load(tsibble, patchwork, tidyverse, zoo, vars, urca, tseries, forecast, broom, readr)


# libraries (manual method) 
#library(readr)
#library(lubridate)
#library(tsibble)
#library(patchwork)
#library(tidyverse) 
#library(zoo) # for quarter year type variables
#library(vars) # package for selecting the optimal lag-order
#library(urca) # package for testing unit roots
#library(ggplot2)
#library(tseries)   # For time series tests (adf.test, kpss.test)
#library(forecast)  # For ARMA modeling and forecasting (auto.arima, forecast, Acf, Pacf)

# functions
# we write down a function to perform tests
perform_tests <- function(series, series_name) {
  series <- na.omit(series) 
  cat("\n--- Testing:", series_name, "---\n")
  # Augmented Dickey-Fuller Test (Unit Root; H0: unit root exists)
  adf_result <- adf.test(series, alternative = "stationary")
  cat("Augmented Dickey-Fuller Test (H0: Unit Root (non-stationary)):\n")
  print(adf_result)
  
  # Kwiatkowski-Phillips-Schmidt-Shin Test (Stationarity; H0: series is stationary)
  kpss_result <- kpss.test(series)
  cat("\n Kwiatkowski-Phillips-Schmidt-Shin Test (H0: Stationarity):\n")
  print(kpss_result)
  
  # Elliott, Rothenberg and Stock Unit Root Test
  ers_result <- ur.ers(series, type = "DF-GLS")
  cat("\n Elliott, Rothenberg and Stock Test (H0: Unit Root (non-stationary)):\n")
  print(summary(ers_result))
  
  # Phillips-Perron Test
  pp_result <- pp.test(series)
  cat("\n Phillips-Perron Test (H0: Unit Root (non-stationary)):\n")
  print(pp_result)
  
  # Comment: Based on p-values, decide if the series appears stationary or has a unit root.
  # If non-stationary, take first difference and re-test.
}

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




tidy_test <- function(obj, test_name) {
  if (inherits(obj, "htest")) {                # ADF, KPSS, PP
    out <- glance(obj) |>
      transmute(statistic = statistic,
                p.value  = p.value,
                test     = test_name)
    
    # Some htests (ADF, KPSS) do not carry critical values.
    # Add NA columns now so the final table is rectangular.
    out$crit_1pct <- out$crit_5pct <- out$crit_10pct <- NA_real_
    
  } else if (inherits(obj, "ur.df") |
             inherits(obj, "ur.ers") |
             inherits(obj, "ur.pp")) {        # ERS, alternative urca tests
    sm <- summary(obj)
    out <- tibble(
      statistic = sm@teststat[1],             # the first column is test-stat
      crit_1pct = sm@cval[1, "1pct"],
      crit_5pct = sm@cval[1, "5pct"],
      crit_10pct= sm@cval[1, "10pct"],
      p.value   = NA_real_,                   # urca summaries give no p-value
      test      = test_name
    )
  } else {
    stop("Unknown test object")
  }
  out
}



perform_tests_new <- function(series,
                          series_name,
                          out_dir = "tables") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  series <- na.omit(series)
  cat("\n--- Testing:", series_name, "---\n")
  
  # ---- 1. Augmented Dickey-Fuller -----------------------------------------
  adf_result <- adf.test(series, alternative = "stationary")
  cat("Augmented Dickey-Fuller Test (H0: unit root):\n")
  print(adf_result)
  
  # ---- 2. KPSS ------------------------------------------------------------
  kpss_result <- kpss.test(series)
  cat("\nKwiatkowski-Phillips-Schmidt-Shin Test (H0: stationarity):\n")
  print(kpss_result)
  
  # ---- 3. Elliott–Rothenberg–Stock DF-GLS ---------------------------------
  ers_result <- ur.ers(series, type = "DF-GLS")
  cat("\nElliott–Rothenberg–Stock DF-GLS Test (H0: unit root):\n")
  print(summary(ers_result))
  
  # ---- 4. Phillips-Perron --------------------------------------------------
  pp_result  <- pp.test(series)
  cat("\nPhillips-Perron Test (H0: unit root):\n")
  print(pp_result)
  
  # ---- Collect results -----------------------------------------------------
  table_out <- bind_rows(
    tidy_test(adf_result,  "ADF"),
    tidy_test(kpss_result, "KPSS"),
    tidy_test(ers_result,  "ERS DF-GLS"),
    tidy_test(pp_result,   "PP")
  ) |>
    relocate(test)
  
  # ---- Write to disk -------------------------------------------------------
  file_csv <- file.path(out_dir,
                        paste0(series_name, "_unitroot_tests.csv"))
  write_csv(table_out, file_csv)
  
  invisible(table_out)   # return the tibble invisibly so you can keep using it
}



identify_estimate_arma_new <- function(series,
                                   series_name,
                                   out_dir = "results") {
  
  ## ------------------------------------------------------------------
  ## 0.  House-keeping -------------------------------------------------
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  ## ------------------------------------------------------------------
  ## 1.  Exploratory ACF / PACF ---------------------------------------
  cat("\n--- Analyzing:", series_name, "---\n")
  cat("Plotting ACF and PACF …\n")
  
  png(file = file.path(out_dir,
                       paste0(series_name, "_ACF_PACF.png")),
      width = 1200, height = 600, res = 120)
  par(mfrow = c(1, 2))
  forecast::Acf(series,  main = paste("ACF –",  series_name))
  forecast::Pacf(series, main = paste("PACF –", series_name))
  dev.off()
  par(mfrow = c(1, 1))        # restore
  
  cat("   → saved to",
      paste0(series_name, "_ACF_PACF.png\n"))
  
  ## ------------------------------------------------------------------
  ## 2.  Automatic ARMA selection -------------------------------------
  cat("Estimating models with auto.arima (AICc selection) …\n")
  best_model <- forecast::auto.arima(series,
                                     stationary   = TRUE,
                                     seasonal     = FALSE,
                                     stepwise     = FALSE,
                                     approximation = FALSE)
  
  cat("Best model selected:\n")
  print(summary(best_model))
  cat("Information criteria  AICc =", best_model$aicc,
      "  AIC =", best_model$aic,
      "  BIC =", best_model$bic, "\n")
  
  ## ------------------------------------------------------------------
  ## 3.  Save coefficient table ---------------------------------------
  coefs <- broom::tidy(best_model)          # coef, std.error, statistic, p.value
  coef_file <- file.path(out_dir,
                         paste0(series_name, "_ARMA_coefs.csv"))
  readr::write_csv(coefs, coef_file)
  cat("   → coefficient table written to",
      basename(coef_file), "\n")
  
  ## ------------------------------------------------------------------
  ## 4.  Residual diagnostics figure ----------------------------------
  cat("Creating residual-diagnostics plot …\n")
  png(file = file.path(out_dir,
                       paste0(series_name, "_residuals.png")),
      width = 1200, height = 800, res = 120)
  forecast::checkresiduals(best_model)
  dev.off()
  cat("   → residual plot saved to",
      paste0(series_name, "_residuals.png\n"))
  
  invisible(best_model)   
}




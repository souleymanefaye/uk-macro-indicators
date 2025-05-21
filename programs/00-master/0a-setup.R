# ==============================================================================
# Macroeconometrics 
# 
# ==============================================================================

# Clear environment
rm(list=ls())

# set the WD to a new folder
root <- setwd("~/Documents/Projects/R/FG2025-uk-macro-indicators") 

# subdirectories
code <- file.path(root, "programs")
work <- file.path(root, "work-data")
raw <- file.path(root, "raw-data")
figures <- file.path(root, "figures")
tables <- file.path(root, "tables")

# libraries 
library(readr)
library(lubridate)
library(tsibble)
library(patchwork)
library(zoo) # for quarter year type variables
library(vars) # package for selecting the optimal lag-order
library(urca) # package for testing unit roots
library(ggplot2)
library(tseries)   # For time series tests (adf.test, kpss.test)
library(forecast)  # For ARMA modeling and forecasting (auto.arima, forecast, Acf, Pacf)

# functions
# we write down a function to perform tests
perform_tests <- function(series, series_name) {
  cat("\n--- Testing:", series_name, "---\n")
  # Augmented Dickey-Fuller Test (Unit Root; H0: unit root exists)
  adf_result <- adf.test(series, alternative = "stationary")
  cat("ADF Test (H0: Unit Root (non-stationary)):\n")
  print(adf_result)
  
  # Kwiatkowski-Phillips-Schmidt-Shin Test (Stationarity; H0: series is stationary)
  kpss_result <- kpss.test(series)
  cat("\nKPSS Test (H0: Stationarity):\n")
  print(kpss_result)
  
  # Elliott, Rothenberg and Stock Unit Root Test
  ers_result <- ur.ers(series, type = "DF-GLS")
  cat("\nKPSS Test (H0: Unit Root (non-stationary)):\n")
  print(ers_result)
  # Phillips-Perron Test
  pp_result <- pp.test(series)
  cat("\nKPSS Test (H0: Unit Root (non-stationary)):\n")
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

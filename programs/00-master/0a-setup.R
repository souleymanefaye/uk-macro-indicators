# ==============================================================================
# Macroeconometrics 
# 
# ==============================================================================

# Clear environment
rm(list=ls())

# set the WD to a new folder
root <- setwd("~/Documents/Projects/R/FG2025-uk-macro-indicators") # Souley's path
#root <- setwd("C:/A Eigene Ordner/Studium/Paris School of Economics/M1/Courses/Econometrics 2b/Homework/uk-macro-indicators") # Gereon's path

# subdirectories
code <- file.path(root, "programs")
work <- file.path(root, "work-data")
raw <- file.path(root, "raw-data")
figures <- file.path(root, "figures")
tables <- file.path(root, "tables")


# Load libraries via pacman
if (!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman")

pacman::p_load(tsibble, patchwork, tidyverse, zoo, vars, urca, tseries, forecast, broom, readr, dplyr, hpfilter, mFilter, xtable, systemfit, stargazer)

# functions
# we write down a function to perform tests
perform_tests <- function(series,
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

identify_estimate_arma <- function(series,
                                   series_name,
                                   out_dir1 = "figures",
                                   out_dir2 = "tables") {
  
  ## ------------------------------------------------------------------
  ## 0.  House-keeping -------------------------------------------------
  dir.create(out_dir1, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir2, showWarnings = FALSE, recursive = TRUE)
  
  ## ------------------------------------------------------------------
  ## 1.  Exploratory ACF / PACF ---------------------------------------
  cat("\n--- Analyzing:", series_name, "---\n")
  cat("Plotting ACF and PACF …\n")
  
  png(file = file.path(out_dir1,
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
  coef_file <- file.path(out_dir2,
                         paste0(series_name, "_ARMA_coefs.csv"))
  readr::write_csv(coefs, coef_file)
  cat("   → coefficient table written to",
      basename(coef_file), "\n")
  
  ## ------------------------------------------------------------------
  ## 4.  Residual diagnostics figure ----------------------------------
  cat("Creating residual-diagnostics plot …\n")
  png(file = file.path(out_dir1,
                       paste0(series_name, "_residuals.png")),
      width = 1200, height = 800, res = 120)
  forecast::checkresiduals(best_model)
  dev.off()
  cat("   → residual plot saved to",
      paste0(series_name, "_residuals.png\n"))
  
  invisible(best_model)   
}





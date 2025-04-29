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

# --------------------- II - Optimal Lag Selection -----------------------------

# --------------------- III - Model Estimation ---------------------------------

# --------------------- IV - Forecasts -----------------------------------------

# ----------------------V - Cholesky Decomposition -----------------------------

# --------------------- VI - Impulse Response Functions ------------------------

# ------------------- VII - Ordering of Variables ---------------------------------
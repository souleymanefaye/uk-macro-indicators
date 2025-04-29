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
library(dplyr)
library(lubridate)
library(tsibble)
library(patchwork)
library(zoo) # for quarter year type variables
library(vars) # package for selecting the optimal lag-order
library(urca) # package for testing unit roots
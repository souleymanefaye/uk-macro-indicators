# ==============================================================================
# Macroeconometrics 
# This master file runs all programs, create data in work folder and figures.
# ==============================================================================

### run this code after running the setup

# Create figures and tables directory if they do not exist 
dir.create(file.path(root, "figures"), showWarnings = FALSE)
dir.create(file.path(root, "tables"), showWarnings = FALSE)
dir.create(file.path(root, "work-data"), showWarnings = FALSE)

# Import and clean dataset
source(file.path(root,"programs/01-import.R"))

# Univariate analysis
source(file.path(root,"programs/02-univariate-analysis.R"))

# Multivariate analysis
source(file.path(root,"programs/03-multivariate-analysis.R"))
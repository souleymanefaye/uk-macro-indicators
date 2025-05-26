# ==============================================================================
# Macroeconometrics 
# This master file runs all programs, create data in work folder and figures.
# ==============================================================================

### run this code after running the setup

# Create figures and tables directory if they do not exist 
dir.create(file.path(root, "figures"), showWarnings = FALSE)
dir.create(file.path(root, "tables"), showWarnings = FALSE)
dir.create(file.path(root, "work-data"), showWarnings = FALSE)

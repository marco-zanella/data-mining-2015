#-----------------------------------------------------------------------
# Analysis of the bike sharing dataset - MARS
# Data Mining - 2015
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
# Obtained from: https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#-----------------------------------------------------------------------
if (!exists('regression_MARS.R')) { regression_MARS.R <- T
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Loads libraries and imports local scripts
library('earth')
source('analysis_preliminary.R')


#-----------------------------------------------------------------------
# Builds models using earth from earth package
# Every variable, degree 1
model <- earth(formula.full, data = training.set, degree = 1)
bike.sharing.MARS.1       <- model
bike.sharing.MARS.1.sqErr <- ComputeMeanSquaredError(model)

# Every variable, degree 2
model <- earth(formula.full, data = training.set, degree = 2)
bike.sharing.MARS.2       <- model
bike.sharing.MARS.2.sqErr <- ComputeMeanSquaredError(model)


# Builds summary table
table.MARS <- data.frame(
  model     = "MARS",
  variables = c("tutte (grado 1)", "tutte (grado 2)"),
  error     = c(
    bike.sharing.MARS.1.sqErr,
    bike.sharing.MARS.2.sqErr
  )
)


#-----------------------------------------------------------------------
# Cleans the workspace
rm(model)

} # endif
#-----------------------------------------------------------------------
# Analysis of the bike sharing dataset - Linear regression
# Data Mining - 2015
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
# Obtained from: https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#-----------------------------------------------------------------------
if (!exists('regression_linear.R')) { regression_linear.R <- T
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Loads libraries and imports local scripts
source('analysis_preliminary.R')


#-----------------------------------------------------------------------
# Computes linear models and squared errors
base                        <- lm(formula.full, data = training.set)
bike.sharing.lm.full        <- step(base, trace = 0)
bike.sharing.lm.full.sqErr  <- ComputeMeanSquaredError(bike.sharing.lm.full)

bike.sharing.lm.short       <- lm(formula.short, data = training.set)
bike.sharing.lm.short.sqErr <- ComputeMeanSquaredError(bike.sharing.lm.short)


# Builds summary table
table.linear <- data.frame(
  model     = "Lineare",
  variables = c("tutte", "solo sign."),
  error     = c(
    bike.sharing.lm.full.sqErr,
    bike.sharing.lm.short.sqErr
  )
)


#-----------------------------------------------------------------------
# Cleans the workspace
rm(base)

} # endif
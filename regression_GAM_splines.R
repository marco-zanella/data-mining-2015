#-----------------------------------------------------------------------
# Analysis of the bike sharing dataset - GAM module with splines
# Data Mining - 2015
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
# Obtained from: https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#-----------------------------------------------------------------------
if (!exists('regression_GAM_splines.R')) { regression_GAM_splines.R <- T
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Loads libraries and imports local scripts
library("gam")
source('analysis_preliminary.R')


#-----------------------------------------------------------------------
# Configuration section
# Spars
kSpars <- seq(0.0, 0.9, length = 15)


#-----------------------------------------------------------------------
# Returns a GAM model where every variable has given spar
GAMSpline <- function(spar) {
  f <- total ~ s(temperature, spar = spar) + s(humidity, spar = spar) +
               s(windspeed, spar = spar) + season + month + hour +
               isHoliday + weekday + weather
  gam(f, data = building.set)
}


# Returns a GAM model with a subset of variables
GAMSplineShort <- function(spar) {
  f <- total ~ s(temperature, spar = spar) + s(humidity, spar = spar) +
               s(windspeed, spar = spar) + season + timeslot +
               weekday + weather
  gam(f, data = building.set)
}


#-----------------------------------------------------------------------
# GAM with every variable
#-----------------------------------------------------------------------
# Scans for spar
results       <- data.frame(ID = 1:length(kSpars))
results$spar  <- 0
results$error <- 0
model         <- NULL
model.error   <- .Machine$integer.max
for (i in 1:length(kSpars)) {
  spar <- kSpars[i]
  g    <- GAMSpline(spar)
  e    <- ComputeMeanSquaredError(g, checking.set)
  
  results$spar[i]  <- spar
  results$error[i] <- e
  
  # Updates best model
  if (e < model.error) {
    model       <- g
    model.error <- e
  }
}


#-----------------------------------------------------------------------
# Saves GAM
bike.sharing.GAM.splines       <- model
bike.sharing.GAM.splines.sqErr <- ComputeMeanSquaredError(model)
bike.sharing.GAM.splines.scan  <- results


#-----------------------------------------------------------------------
# GAM with subset of variables
#-----------------------------------------------------------------------
# Scans for spar
results       <- data.frame(ID = 1:length(kSpars))
results$spar  <- 0
results$error <- 0
model         <- NULL
model.error   <- .Machine$integer.max
for (i in 1:length(kSpars)) {
  spar <- kSpars[i]
  g    <- GAMSplineShort(spar)
  e    <- ComputeMeanSquaredError(g, checking.set)
  
  results$spar[i]  <- spar
  results$error[i] <- e
  
  # Updates best model
  if (e < model.error) {
    model       <- g
    model.error <- e
  }
}


#-----------------------------------------------------------------------
# Saves GAM
bike.sharing.GAM.splines.short       <- model
bike.sharing.GAM.splines.short.sqErr <- ComputeMeanSquaredError(model)
bike.sharing.GAM.splines.short.scan  <- results


#-----------------------------------------------------------------------
# Builds summary table
table.GAM.splines <- data.frame(
  model     = c("GAM (splines)", "GAM (splines)"),
  variables = c("tutte", "solo sign."),
  error     = c(
    bike.sharing.GAM.splines.sqErr,
    bike.sharing.GAM.splines.short.sqErr
  )
)


#-----------------------------------------------------------------------
# Clears the workspace
rm(kSpars)
rm(GAMSpline, GAMSplineShort)
rm(results, model, model.error, spar, g, e, i)

} # endif
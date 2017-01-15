#-----------------------------------------------------------------------
# Analysis of the bike sharing dataset - GAM module with loess
# Data Mining - 2015
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
# Obtained from: https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#-----------------------------------------------------------------------
if (!exists('regression_GAM_loess.R')) { regression_GAM_loess.R <- T
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Loads libraries and imports local scripts
library("gam")
source('analysis_preliminary.R')


#-----------------------------------------------------------------------
# Configuration section
# spans
kSpans <- seq(0.2, 0.9, length = 15)


#-----------------------------------------------------------------------
# Returns a GAM model where every variable has given span
GAMLoess <- function(span) {
  f <- total ~ lo(temperature, span = span) + humidity +
               lo(windspeed, span = span) + season + month + hour +
               isHoliday + weekday + weather
  gam(f, data = building.set)
}


# Returns a GAM model with a subset of variables
GAMLoessShort <- function(span) {
  f <- total ~ lo(temperature, span = span) + humidity +
               lo(windspeed, span = span) + season + timeslot +
               weekday + weather
  gam(f, data = building.set)
}


#-----------------------------------------------------------------------
# GAM with every variable
#-----------------------------------------------------------------------
# Scans for span
results       <- data.frame(ID = 1:length(kSpans))
results$span  <- 0
results$error <- 0
model         <- NULL
model.error   <- .Machine$integer.max
for (i in 1:length(kSpans)) {
  span <- kSpans[i]
  g    <- GAMLoess(span)
  e    <- ComputeMeanSquaredError(g, checking.set)
  
  results$span[i]  <- span
  results$error[i] <- e
  
  # Updates best model
  if (e < model.error) {
    model       <- g
    model.error <- e
  }
}


#-----------------------------------------------------------------------
# Saves GAM
bike.sharing.GAM.loess       <- model
bike.sharing.GAM.loess.sqErr <- ComputeMeanSquaredError(model)
bike.sharing.GAM.loess.scan  <- results


#-----------------------------------------------------------------------
# GAM with subset of variables
#-----------------------------------------------------------------------
# Scans for span
results       <- data.frame(ID = 1:length(kSpans))
results$span  <- 0
results$error <- 0
model         <- NULL
model.error   <- .Machine$integer.max
for (i in 1:length(kSpans)) {
  span <- kSpans[i]
  g    <- GAMLoessShort(span)
  e    <- ComputeMeanSquaredError(g, checking.set)
  
  results$span[i]  <- span
  results$error[i] <- e
  
  # Updates best model
  if (e < model.error) {
    model       <- g
    model.error <- e
  }
}


#-----------------------------------------------------------------------
# Saves GAM
bike.sharing.GAM.loess.short       <- model
bike.sharing.GAM.loess.short.sqErr <- ComputeMeanSquaredError(model)
bike.sharing.GAM.loess.short.scan  <- results


#-----------------------------------------------------------------------
# Builds summary table
table.GAM.loess <- data.frame(
  model     = c("GAM (loess)", "GAM (loess)"),
  variables = c("tutte", "solo sign."),
  error     = c(
    bike.sharing.GAM.loess.sqErr,
    bike.sharing.GAM.loess.short.sqErr
  )
)


#-----------------------------------------------------------------------
# Clears the workspace
rm(kSpans)
rm(GAMLoess, GAMLoessShort)
rm(results, model, model.error, span, g, e, i)

} # endif
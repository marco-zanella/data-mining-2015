#-----------------------------------------------------------------------
# Analysis of the bike sharing dataset - Projection Pursuit Regression
# Data Mining - 2015
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
# Obtained from: https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#-----------------------------------------------------------------------
if (!exists('regression_PPR.R')) { regression_PPR.R <- T
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Loads libraries and imports local scripts
library('nnet')
source('analysis_preliminary.R')


#-----------------------------------------------------------------------
# Configuration section
kNterms <- 2:6
kSpans  <- seq(0.1, 0.6, length = 10)


#-----------------------------------------------------------------------
# Returns a Projection Pursuit Model
PPR <- function (nterms = 1, span = 0.1) {
  ppr(formula.full, data = building.set, nterms = nterms, span = span)
}


#-----------------------------------------------------------------------
# Scans for number of terms and span
results       <- data.frame(ID = 1:(length(kNterms) * length(kSpans)))
results$nterm <- 0
results$span  <- 0
results$error <- 0
model         <- NULL
model.error   <- .Machine$integer.max
for (i in 1:length(kNterms)) {
  n <- kNterms[i]
  for (j in 1:length(kSpans)) {
    s   <- kSpans[j]
    idx <- (i - 1) * length(kSpans) + j
    p   <- PPR(n, s)
    e   <- ComputeMeanSquaredError(p, checking.set)
    
    results$nterm[idx] <- n
    results$span[idx]  <- s
    results$error[idx] <- e
    
    # Updates best model
    if (e < model.error) {
      model       <- p
      model.error <- e
    }
  }
}


#-----------------------------------------------------------------------
# Saves neural network
bike.sharing.PPR       <- model
bike.sharing.PPR.sqErr <- ComputeMeanSquaredError(model)
bike.sharing.PPR.scan  <- results

# Builds summary table
table.PPR <- data.frame(
  model     = "PPR",
  variables = paste(model$mu, " termini"),
  error     = model.error
)


#-----------------------------------------------------------------------
# Clears the workspace
rm(kNterms, kSpans)
rm(PPR)
rm(results, model, model.error)
rm(n, s, idx, p, e)

} # endif
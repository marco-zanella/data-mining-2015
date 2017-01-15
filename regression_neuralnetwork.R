#-----------------------------------------------------------------------
# Analysis of the bike sharing dataset - Neural network
# Data Mining - 2015
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
# Obtained from: https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#-----------------------------------------------------------------------
if (!exists('regression_neuralnetwork.R')) { regression_neuralnetwork.R <- T
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Loads libraries and imports local scripts
library('nnet')
source('analysis_preliminary.R')


#-----------------------------------------------------------------------
# Configuration section
kSizes  <- 4:6
kDecays <- 10 ^ seq(-1, -3, length = 3)


#-----------------------------------------------------------------------
# Returns a neural network
NeuralNetwork <- function(size, decay, maxit = 2000) {
  nnet(
    formula.full,
    size   = size,
    decay  = decay,
    data   = building.set,
    linout = T,
    maxit  = maxit,
    trace  = F
  )
}


#-----------------------------------------------------------------------
# Scans sizes and decays, and finds best model
results       <- data.frame(ID = 1:(length(kSizes) * length(kDecays)))
results$size  <- 0
results$decay <- 0
results$error <- 0
model         <- NULL
model.error   <- .Machine$integer.max
for (i in 1:length(kSizes)) {
    s <- kSizes[i]
    for (j in 1:length(kDecays)) {
      d   <- kDecays[j]
      idx <- (i - 1) * length(kDecays) + j
      n   <- NeuralNetwork(s, d)
      e   <- ComputeMeanSquaredError(n, checking.set)
      
      results$size[idx]  <- s
      results$decay[idx] <- d
      results$error[idx] <- e
      
      # Updates best model
      if (e < model.error) {
        model       <- n
        model.error <- e
      }
    }
}


#-----------------------------------------------------------------------
# Saves neural network
bike.sharing.neuralnetwork       <- model
bike.sharing.neuralnetwork.sqErr <- ComputeMeanSquaredError(model)
bike.sharing.neuralnetwork.scan  <- results

# Builds summary table
table.neuralnetwork <- data.frame(
  model     = "Rete neurale",
  variables = "-",
  error     = model.error
)


#-----------------------------------------------------------------------
# Clears the workspace
rm(kSizes, kDecays)
rm(NeuralNetwork)
rm(i, s, j, d, idx, n, e, results)
rm(model, model.error)

} # endif
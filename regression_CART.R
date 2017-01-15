#-----------------------------------------------------------------------
# Analysis of the bike sharing dataset - CART
# Data Mining - 2015
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
# Obtained from: https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#-----------------------------------------------------------------------
if (!exists('regression_CART.R')) { regression_CART.R <- T
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Loads libraries and imports local scripts
library('tree')
source('analysis_preliminary.R')


#-----------------------------------------------------------------------
# Configuration section
kMaxPseudoDepth <- 31


#-----------------------------------------------------------------------
# Support functions
# Returns number of leaves in  a tree
nleaves <- function(t) {
  NROW(t$frame$var[t$frame$var == '<leaf>'])
}


#-----------------------------------------------------------------------
# Scans pseudo dephts
tree.original <- NULL
error.old     <- 0
for (h in 1:kMaxPseudoDepth) {
  tree.original <- tree(
    formula.full,
    building.set,
    mincut  = NROW(training.set) / 2^(h),
    minsize = NROW(training.set) / 2^(h - 1),
    mindev  = 0.001
  )
  error <- ComputeMeanSquaredError(tree.original, checking.set)
  
  # Stops if converged
  if (error == error.old)
    break
  error.old <- error
}


# Saves tree
bike.sharing.CART.full       <- tree.original
bike.sharing.CART.full.sqErr <- ComputeMeanSquaredError(tree.original)


# Computes scores using cross validation
tree.cv       <- cv.tree(tree.original)
tree.cv$score <- tree.cv$dev + log(tree.cv$size)


# Prunes the tree
size         <- tree.cv$size[which.min(tree.cv$score)]
tree.pruned  <- prune.tree(tree.original, best = size)
error.pruned <- ComputeMeanSquaredError(tree.pruned)


# Saves pruned tree
bike.sharing.CART.pruned       <- tree.pruned
bike.sharing.CART.pruned.sqErr <- error.pruned
bike.sharing.CART.scan         <- data.frame(size = tree.cv$size, dev = tree.cv$dev)


# Builds summary table
table.CART <- data.frame(
  model     = c("CART", "CART"),
  variables = c(
    paste(nleaves(bike.sharing.CART.full), " foglie"),
    paste(nleaves(bike.sharing.CART.pruned), " foglie")
  ),
  error     = c(
    bike.sharing.CART.full.sqErr,
    bike.sharing.CART.pruned.sqErr
  )
)


#-----------------------------------------------------------------------
# Cleans the workspace
rm(kMaxPseudoDepth)
rm(tree.original,h, error, error.old)
rm(tree.cv, size, tree.pruned, error.pruned)

} # endif
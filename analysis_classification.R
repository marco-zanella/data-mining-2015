#-----------------------------------------------------------------------
# Analysis of the bike sharing dataset - Fully automated analysis
# Data Mining - 2015
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
# Obtained from: https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#-----------------------------------------------------------------------

# Loads libraries and local scripts
library('tree')
library('earth')
library('adabag')
library('ipred')
library('randomForest')
source('R/lift.R')
source('R/roc.R')
source('R/plot_pdf.R')

# Loads data set and performs preliminary analysis
source('analysis_preliminary.R')



#-----------------------------------------------------------------------
# Computes probabilities and removes where P = 0 or P = 1
set.seed(1079762)
bike         <- bike.sharing
bike$pCasual <- bike$casual / bike$total
bike         <- bike[bike$pCasual > 0 & bike$pCasual < 1, ]


# Sets thresholds for high and low
qnt  <- quantile(bike$pCasual, c(0.25, 0.75))
low  <- qnt[1]
high <- qnt[2]
rm(qnt)


# Converts from factor to ordered
Factor2Ordered <- function (factors) {
  factor(factors, levels = c('low', 'medium', 'high'), ordered = T)
}

# Converts from ordered to factor
Ordered2Factor <- function (ordereds) {
  factor(ordereds, ordered = F)
}


# Tells given probability can be classified as 'high'
isHigh <- function (probability) {
  if (is.numeric(probability))
    probability > high
  else
    probability == 'high'
}

# Tells whether probability can be classified as 'low'
isLow <- function (probability) {
  if (is.numeric(probability))
    probability < low
  else
    probability == 'low'
}

# Tells whether probability can be classified as 'medium'
isMedium <- function (probability) {
  !isLow(probability) & !isHigh(probability)
}


# Classifies probability as 'high', 'medium' or 'low'
Classify <- function (probability) {
  class <- vector(mode = 'character', length = length(probability))
  class[isHigh(probability)]   <- 'high'
  class[isMedium(probability)] <- 'medium'
  class[isLow(probability)]    <- 'low'
  Factor2Ordered(class)
}


# Adds information about class
bike$class <- Classify(bike$pCasual)



#-----------------------------------------------------------------------
# Prepares training and verify sets
random.sample <- sample(1:NROW(bike), 0.75 * NROW(bike))
training      <- bike[random.sample, ]
verify        <- bike[-random.sample, ]
rm(random.sample)



#-----------------------------------------------------------------------
# Predicts probabilities on the verify set
Predict <- function(model) {
  predict(model, newdata = verify)
}

# Predicts classes on the verify set
PredictClass <- function (model, probabilistic = T) {
  if (probabilistic) {
    Classify(predict(model, newdata = verify))
  } else {
    predict(model, newdata = verify, type = 'class')
  }
}



#-----------------------------------------------------------------------
# Confusion matrix
TotalError <- function(classification, observation = verify$class) {
  confusion.table <- table(classification, observation)
  1 - sum(diag(confusion.table)) / sum(confusion.table)
}

ConfusionMatrix <- function(classification, observation = verify$class) {
  confusion.table <- table(classification, observation)
  error           <- TotalError(confusion.table)
  
  print(confusion.table)
  print(paste("Total error:", error))
}



#-----------------------------------------------------------------------
# Ready to use functions
f_class <- class ~ hour + weekday + isHoliday + weather + month
f_num   <- pCasual ~ hour + weekday + isHoliday + weather + month


#-----------------------------------------------------------------------
# Performs analyses
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Linear model on probability
linear.model       <- lm(f_num, data = training)
linear.model.class <- PredictClass(linear.model)



#-----------------------------------------------------------------------
# Classification CART
classification.tree       <- tree(f_class, data = training)
classification.tree.class <- PredictClass(classification.tree, probabilistic = F)



#-----------------------------------------------------------------------
# Regression CART
regression.tree       <- tree(f_num, data = training)
regression.tree.class <- PredictClass(regression.tree)



#-----------------------------------------------------------------------
# MARS
mars.model       <- earth(f_num, data = training)
mars.model.class <- PredictClass(mars.model)



#-----------------------------------------------------------------------
# Bagging
bagging.model       <- bagging(f_class, data = training, nbagg = 50)
bagging.model.class <- PredictClass(bagging.model)



#-----------------------------------------------------------------------
# Boosting - NOTE: training2 is required because this implementation of
#                  boosting does not handle ordered
training2            <- training
training2$class      <- Ordered2Factor(training2$class)
boosting.model       <- boosting(f_class, data = training2, mfinal = 10)
boosting.model.class <- Factor2Ordered(Predict(boosting.model)$class)
rm(training2)



#-----------------------------------------------------------------------
# Random random
random.forest       <- randomForest(f_num, data = training, ntree = 50, nodesize = 5, mtry = 2)
random.forest.class <- PredictClass(random.forest)



#-----------------------------------------------------------------------
# Prepares data for lift plots
#-----------------------------------------------------------------------
# Linear model
lift1.low    <- lift(isLow(linear.model.class),    isLow(verify$class),    draw = F, mode = 'smooth')
lift1.medium <- lift(isMedium(linear.model.class), isMedium(verify$class), draw = F, mode = 'smooth')
lift1.high   <- lift(isHigh(linear.model.class),   isHigh(verify$class),   draw = F, mode = 'smooth')

# Classification CART
lift2.low    <- lift(isLow(classification.tree.class),    isLow(verify$class),    draw = F, mode = 'smooth')
lift2.medium <- lift(isMedium(classification.tree.class), isMedium(verify$class), draw = F, mode = 'smooth')
lift2.high   <- lift(isHigh(classification.tree.class),   isHigh(verify$class),   draw = F, mode = 'smooth')

# Regression CART
lift3.low    <- lift(isLow(regression.tree.class),    isLow(verify$class),    draw = F, mode = 'smooth')
lift3.medium <- lift(isMedium(regression.tree.class), isMedium(verify$class), draw = F, mode = 'smooth')
lift3.high   <- lift(isHigh(regression.tree.class),   isHigh(verify$class),   draw = F, mode = 'smooth')

# MARS
lift4.low    <- lift(isLow(mars.model.class),    isLow(verify$class),    draw = F, mode = 'smooth')
lift4.medium <- lift(isMedium(mars.model.class), isMedium(verify$class), draw = F, mode = 'smooth')
lift4.high   <- lift(isHigh(mars.model.class),   isHigh(verify$class),   draw = F, mode = 'smooth')

# Bagging
lift5.low    <- lift(isLow(bagging.model.class),    isLow(verify$class),    draw = F, mode = 'smooth')
lift5.medium <- lift(isMedium(bagging.model.class), isMedium(verify$class), draw = F, mode = 'smooth')
lift5.high   <- lift(isHigh(bagging.model.class),   isHigh(verify$class),   draw = F, mode = 'smooth')

# Boosting
lift6.low    <- lift(isLow(boosting.model.class),    isLow(verify$class),    draw = F, mode = 'smooth')
lift6.medium <- lift(isMedium(boosting.model.class), isMedium(verify$class), draw = F, mode = 'smooth')
lift6.high   <- lift(isHigh(boosting.model.class),   isHigh(verify$class),   draw = F, mode = 'smooth')

# Random forest
lift7.low    <- lift(isLow(random.forest.class),    isLow(verify$class),    draw = F, mode = 'smooth')
lift7.medium <- lift(isMedium(random.forest.class), isMedium(verify$class), draw = F, mode = 'smooth')
lift7.high   <- lift(isHigh(random.forest.class),   isHigh(verify$class),   draw = F, mode = 'smooth')


# Plots lift fot high prediction
PlotLiftHigh <- function() {
  plot(lift1.high, type='l', col = 1, pch = 1,
       xlim = c(0, 1), ylim=c(1, 3.5),
       xlab = "Frazione di soggetti previsti",
       ylab = "Fattore di miglioramento",
       main = "Lift - classificazione di 'high'"
  )
  lines(lift2.high, type = 'l', col = 2, lty = 2)
  lines(lift3.high, type = 'l', col = 3, lty = 3)
  lines(lift4.high, type = 'l', col = 4, lty = 1)
  lines(lift5.high, type = 'l', col = 5, lty = 2)
  lines(lift6.high, type = 'l', col = 6, lty = 3)
  lines(lift7.high, type = 'l', col = 7, lty = 1)

  legend(0.5, 3.5, c("modello lineare", "CART di classificazione", "CART di regressione", "MARS", "bagging", "boosting", "random forest"),
    col = c(1, 2, 3, 4, 5, 6, 7),
    lty = c(1, 2, 3, 1, 2, 3, 1)
  )
}


# Plots lift fot medium prediction
PlotLiftMedium <- function() {
  plot(lift1.medium, type='l', col = 1, pch = 1,
       xlim = c(0, 1), ylim=c(1, 1.7),
       xlab = "Frazione di soggetti previsti",
       ylab = "Fattore di miglioramento",
       main = "Lift - classificazione di 'medium'"
  )
  lines(lift2.medium, type = 'l', col = 2, lty = 2)
  lines(lift3.medium, type = 'l', col = 3, lty = 3)
  lines(lift4.medium, type = 'l', col = 4, lty = 1)
  lines(lift5.medium, type = 'l', col = 5, lty = 2)
  lines(lift6.medium, type = 'l', col = 6, lty = 3)
  lines(lift7.medium, type = 'l', col = 7, lty = 1)

  legend(0.5, 1.7, c("modello lineare", "CART di classificazione", "CART di regressione", "MARS", "bagging", "boosting", "random forest"),
    col = c(1, 2, 3, 4, 5, 6, 7),
    lty = c(1, 2, 3, 1, 2, 3, 1)
  )
}


# Plots lift fot low prediction
PlotLiftLow <- function() {
  plot(lift1.low, type='l', col = 1, pch = 1,
       xlim = c(0, 1), ylim=c(1, 4),
       xlab = "Frazione di soggetti previsti",
       ylab = "Fattore di miglioramento",
       main = "Lift - classificazione di 'low'"
  )
  lines(lift2.low, type = 'l', col = 2, lty = 2)
  lines(lift3.low, type = 'l', col = 3, lty = 3)
  lines(lift4.low, type = 'l', col = 4, lty = 1)
  lines(lift5.low, type = 'l', col = 5, lty = 2)
  lines(lift6.low, type = 'l', col = 6, lty = 3)
  lines(lift7.low, type = 'l', col = 7, lty = 1)

  legend(0.5, 4, c("modello lineare", "CART di classificazione", "CART di regressione", "MARS", "bagging", "boosting", "random forest"),
    col = c(1, 2, 3, 4, 5, 6, 7),
    lty = c(1, 2, 3, 1, 2, 3, 1)
  )
}



#-----------------------------------------------------------------------
# Prepares data for ROC plots
#-----------------------------------------------------------------------
# Linear model
roc1.low    <- roc(isLow(linear.model.class),    isLow(verify$class),    draw = F, mode = 'smooth')
roc1.medium <- roc(isMedium(linear.model.class), isMedium(verify$class), draw = F, mode = 'smooth')
roc1.high   <- roc(isHigh(linear.model.class),   isHigh(verify$class),   draw = F, mode = 'smooth')

# Classification CART
roc2.low    <- roc(isLow(classification.tree.class),    isLow(verify$class),    draw = F, mode = 'smooth')
roc2.medium <- roc(isMedium(classification.tree.class), isMedium(verify$class), draw = F, mode = 'smooth')
roc2.high   <- roc(isHigh(classification.tree.class),   isHigh(verify$class),   draw = F, mode = 'smooth')

# Regression CART
roc3.low    <- roc(isLow(regression.tree.class),    isLow(verify$class),    draw = F, mode = 'smooth')
roc3.medium <- roc(isMedium(regression.tree.class), isMedium(verify$class), draw = F, mode = 'smooth')
roc3.high   <- roc(isHigh(regression.tree.class),   isHigh(verify$class),   draw = F, mode = 'smooth')

# MARS
roc4.low    <- roc(isLow(mars.model.class),    isLow(verify$class),    draw = F, mode = 'smooth')
roc4.medium <- roc(isMedium(mars.model.class), isMedium(verify$class), draw = F, mode = 'smooth')
roc4.high   <- roc(isHigh(mars.model.class),   isHigh(verify$class),   draw = F, mode = 'smooth')

# Bagging
roc5.low    <- roc(isLow(bagging.model.class),    isLow(verify$class),    draw = F, mode = 'smooth')
roc5.medium <- roc(isMedium(bagging.model.class), isMedium(verify$class), draw = F, mode = 'smooth')
roc5.high   <- roc(isHigh(bagging.model.class),   isHigh(verify$class),   draw = F, mode = 'smooth')

# Boosting
roc6.low    <- roc(isLow(boosting.model.class),    isLow(verify$class),    draw = F, mode = 'smooth')
roc6.medium <- roc(isMedium(boosting.model.class), isMedium(verify$class), draw = F, mode = 'smooth')
roc6.high   <- roc(isHigh(boosting.model.class),   isHigh(verify$class),   draw = F, mode = 'smooth')

# Random forest
roc7.low    <- roc(isLow(random.forest.class),    isLow(verify$class),    draw = F, mode = 'smooth')
roc7.medium <- roc(isMedium(random.forest.class), isMedium(verify$class), draw = F, mode = 'smooth')
roc7.high   <- roc(isHigh(random.forest.class),   isHigh(verify$class),   draw = F, mode = 'smooth')


# Plots roc fot high prediction
PlotRocHigh <- function() {
  plot(roc1.high, type='l', col = 1, pch = 1,
       xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Specificità",
       ylab = "Sensibilità",
       main = "ROC - classificazione di 'high'"
  )
  lines(roc2.high, type = 'l', col = 2, lty = 2)
  lines(roc3.high, type = 'l', col = 3, lty = 3)
  lines(roc4.high, type = 'l', col = 4, lty = 1)
  lines(roc5.high, type = 'l', col = 5, lty = 2)
  lines(roc6.high, type = 'l', col = 6, lty = 3)
  lines(roc7.high, type = 'l', col = 7, lty = 1)
  abline(0, 1, col = 1, lty = 2)

  legend(0.55, 0.35, c("modello lineare", "CART di classificazione", "CART di regressione", "MARS", "bagging", "boosting", "random forest"),
    col = c(1, 2, 3, 4, 5, 6, 7),
    lty = c(1, 2, 3, 1, 2, 3, 1)
  )
}


# Plots roc fot medium prediction
PlotRocMedium <- function() {
  plot(roc1.medium, type='l', col = 1, pch = 1,
       xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Specificità",
       ylab = "Sensibilità",
       main = "ROC - classificazione di 'medium'"
  )
  lines(roc2.medium, type = 'l', col = 2, lty = 2)
  lines(roc3.medium, type = 'l', col = 3, lty = 3)
  lines(roc4.medium, type = 'l', col = 4, lty = 1)
  lines(roc5.medium, type = 'l', col = 5, lty = 2)
  lines(roc6.medium, type = 'l', col = 6, lty = 3)
  lines(roc7.medium, type = 'l', col = 7, lty = 1)
  abline(0, 1, col = 1, lty = 2)

  legend(0.55, 0.35, c("modello lineare", "CART di classificazione", "CART di regressione", "MARS", "bagging", "boosting", "random forest"),
    col = c(1, 2, 3, 4, 5, 6, 7),
    lty = c(1, 2, 3, 1, 2, 3, 1)
  )
}


# Plots roc fot low prediction
PlotRocLow <- function() {
  plot(roc1.low, type='l', col = 1, pch = 1,
       xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Specificità",
       ylab = "Sensibilità",
       main = "ROC - classificazione di 'low'"
  )
  lines(roc2.low, type = 'l', col = 2, lty = 2)
  lines(roc3.low, type = 'l', col = 3, lty = 3)
  lines(roc4.low, type = 'l', col = 4, lty = 1)
  lines(roc5.low, type = 'l', col = 5, lty = 2)
  lines(roc6.low, type = 'l', col = 6, lty = 3)
  lines(roc7.low, type = 'l', col = 7, lty = 1)
  abline(0, 1, col = 1, lty = 2)

  legend(0.55, 0.35, c("modello lineare", "CART di classificazione", "CART di regressione", "MARS", "bagging", "boosting", "random forest"),
    col = c(1, 2, 3, 4, 5, 6, 7),
    lty = c(1, 2, 3, 1, 2, 3, 1)
  )
}



#-----------------------------------------------------------------------
# Exports plots
PlotPDF("plots/lift-high.pdf",   PlotLiftHigh)
PlotPDF("plots/lift-medium.pdf", PlotLiftMedium)
PlotPDF("plots/lift-low.pdf",    PlotLiftLow)

PlotPDF("plots/roc-high.pdf",   PlotRocHigh)
PlotPDF("plots/roc-medium.pdf", PlotRocMedium)
PlotPDF("plots/roc-low.pdf",    PlotRocLow)


#-----------------------------------------------------------------------
# Prints total errors
print("Total errors:")
print(paste("Linear model:       ", TotalError(linear.model.class)))
print(paste("Classification CART:", TotalError(classification.tree.class)))
print(paste("Regression CART:    ", TotalError(regression.tree.class)))
print(paste("MARS:               ", TotalError(mars.model.class)))
print(paste("Bagging:            ", TotalError(bagging.model.class)))
print(paste("Boosting:           ", TotalError(boosting.model.class)))
print(paste("Random forest:      ", TotalError(random.forest.class)))
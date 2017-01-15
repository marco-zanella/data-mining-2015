#-----------------------------------------------------------------------
# Functions to plot stuffs.
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
#-----------------------------------------------------------------------
if (!exists('plot_functions.R')) { plot_functions.R <- T
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#' Plots a group of plots.
#'
#' Plots graphical outputs of every function in the list. Plots are
#' arranged into a grid.
#'
#' @param list          List of functions producing plots
#' @return Nothing
#'
#' @examples
#' PlotN(c(foo, bar))
PlotN <- function(list) {
  n    <- NROW(list)
  cols <- ceiling(sqrt(n))
  rows <- ceiling(n / cols)
  
  settings.old <- par(mfrow = c(rows, cols))
  for (i in 1:n) {
    list[[i]]()
  }
  par(settings.old)
}


#-----------------------------------------------------------------------
# Plots feeling temperature versus temperature
PlotFeelingVsTempByDay <- function() {
  source('analysis_preliminary.R')
  daily <- data.frame(unique(bike.sharing$date))
  names(daily)[1] <- "date"
  
  daily$temperature <- 0.0
  daily$feelingTemp <- 0.0
  for (i in 1:NROW(daily)) {
    idx                  <- bike.sharing$date == daily$date[i]
    daily$temperature[i] <- mean(bike.sharing$temperature[idx])
    daily$feelingTemp[i] <- mean(bike.sharing$feelingTemp[idx])
  }
  
  plot(feelingTemp ~ temperature, data = daily,
       main = "Temperatura percepita vs temperatura",
       xlab = "temperatura (°C)", ylab = "temperatura percepita (°C)")
}


#-----------------------------------------------------------------------
# Plots humidity
PlotHumidityByDay <- function() {
  source('analysis_preliminary.R')
  daily <- data.frame(unique(bike.sharing$date))
  names(daily)[1] <- "date"
  
  daily$humidity <- 0.0
  for (i in 1:NROW(daily)) {
    daily$humidity[i] <- mean(bike.sharing$humidity[bike.sharing$date == daily$date[i]])
  }
  
  plot(humidity ~ date, data = daily,
       main = "Umidità relativa", xlab = "data", ylab = "% umidità")
}


#-----------------------------------------------------------------------
# Plots total number of users versus month
PlotTotalVsMonth <- function() {
  source('analysis_preliminary.R')
  boxplot(total ~ month, data = bike.sharing,
          main = "Utenti vs mese", xlab = "mese", ylab = "utenti")
}


# Plots total number of users versus season
PlotTotalVsSeason <- function() {
  source('analysis_preliminary.R')
  boxplot(total ~ season, data = bike.sharing,
          main = "Utenti vs stagione", xlab = "stagione", ylab = "utenti")
}


#-----------------------------------------------------------------------
# Plots total number of users versus hour
PlotTotalVsHour <- function() {
  source('analysis_preliminary.R')
  boxplot(total ~ hour, data = bike.sharing,
          main = "Utenti vs orario", xlab = "ora", ylab = "utenti")
}


# Plots total number of users versus timeslot
PlotTotalVsTimeslot <- function() {
  source('analysis_preliminary.R')
  boxplot(total ~ timeslot, data = bike.sharing,
        main = "Utenti vs fascia di utilizzo", xlab = "fascia di utilizzo", ylab = "utenti")
}


#-----------------------------------------------------------------------
# Plots GAM splines full scan
PlotGAMSplinesFullScan <- function() {
  source('regression_GAM_splines.R')
  scan <- bike.sharing.GAM.splines.scan
  plot(error ~ spar, data = scan, main = "Scansione GAM splines (tutte le variabili)",
  xlab = "lisciamento", ylab = "errore", type="b", lty = 2)
}


#-----------------------------------------------------------------------
# Plots GAM splines short scan
PlotGAMSplinesShortScan <- function() {
  source('regression_GAM_splines.R')
  scan <- bike.sharing.GAM.splines.short.scan
  plot(error ~ spar, data = scan, main = "Scansione GAM splines (solo variabili significative)",
  xlab = "lisciamento", ylab = "errore", type="b", lty = 2)
}


#-----------------------------------------------------------------------
# Plots GAM loess full scan
PlotGAMLoessFullScan <- function() {
  source('regression_GAM_loess.R')
  scan <- bike.sharing.GAM.loess.scan
  plot(error ~ span, data = scan, main = "Scansione GAM loess (tutte le variabili)",
  xlab = "lisciamento", ylab = "errore", type="b", lty = 2)
}


#-----------------------------------------------------------------------
# Plots GAM loess short scan
PlotGAMLoessShortScan <- function() {
  source('regression_GAM_loess.R')
  scan <- bike.sharing.GAM.loess.short.scan
  plot(error ~ span, data = scan, main = "Scansione GAM loess (solo variabili significative)",
  xlab = "lisciamento", ylab = "errore", type="b", lty = 2)
}


#-----------------------------------------------------------------------
# Plots the PPR scan
PlotPPRScan <- function() {
  library('lattice')
  source('regression_PPR.R')
  scan <- bike.sharing.PPR.scan
  print(wireframe(error ~ span * nterm, data = scan, main = "Scansione PPR",
            xlab = "lisciamento", ylab = "termini", zlab = "errore"))
}


#-----------------------------------------------------------------------
# Plots neural network scan
PlotNeuralNetworkScan <- function() {
  library('lattice')
  source('regression_neuralnetwork.R')
  scan <- bike.sharing.neuralnetwork.scan
  print(wireframe(error ~ size * decay, data = scan, main = "Scansione rete neurale",
            xlab = "dim. strato latente", ylab = "decadimento", zlab = "errore"))
}


#-----------------------------------------------------------------------
# Plots CART tree (original)
PlotCART <- function() {
  source('regression_CART.R')
  plot(bike.sharing.CART.full, main = "Albero CART")
  text(bike.sharing.CART.full, pretty = 0, cex = 0.75)
}


#-----------------------------------------------------------------------
# Plots CART tree (pruned)
PlotCARTPruned <- function() {
  source('regression_CART.R')
  plot(bike.sharing.CART.pruned, main = "Albero CART potato")
  text(bike.sharing.CART.pruned, pretty = 0, cex = 0.75)
}


#-----------------------------------------------------------------------
# Plots CART scan
PlotCARTScan <- function() {
  source('regression_CART.R')
  scan <- bike.sharing.CART.scan
  plot(dev ~ size, data = scan, main = "CART - devianza vs dimensione",
       xlab = "dimensione", ylab = "devianza", type = "b", lty = 2)
}

} # endif
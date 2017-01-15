#-----------------------------------------------------------------------
# Analysis of the bike sharing dataset - Fully automated analysis
# Data Mining - 2015
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
# Obtained from: https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#-----------------------------------------------------------------------

# Checks dependencies
source('dependencies.R')

# Loads data set and performs preliminary analysis
source('analysis_preliminary.R')
source('R/export_table.R')

#-----------------------------------------------------------------------
# Regression

# Linear regression
print("Eseguo regressione lineare...")
source('regression_linear.R')

# GAM using splines
print("Eseguo regressione con GAM - splines...")
source('regression_GAM_splines.R')

# GAM using loess
print("Eseguo regressione con GAM - loess...")
source('regression_GAM_loess.R')

# Projection Pursuit Regression
print("Eseguo regressione Projection Pursuit...")
source('regression_PPR.R')

# Regression with MARS
print("Eseguo regressione con MARS...")
source('regression_MARS.R')

# Regression with neural network
print("Eseguo regressione con rete neurale...")
source('regression_neuralnetwork.R')

# Regression with CART trees
print("Eseguo regressione con albero CART...")
source('regression_CART.R')


#-----------------------------------------------------------------------
# Exports tables
print("Esporto le tabelle in output/...")
ExportTable(table.linear,        "output/table_linear.tex")
ExportTable(table.GAM.splines,   "output/table_GAM_splines.tex")
ExportTable(table.GAM.loess,     "output/table_GAM_loess.tex")
ExportTable(table.PPR,           "output/table_PPR.tex")
ExportTable(table.MARS,          "output/table_MARS.tex")
ExportTable(table.neuralnetwork, "output/table_neuralnetwork.tex")
ExportTable(table.CART,          "output/table_CART.tex")


#-----------------------------------------------------------------------
# Exports plots
print("Esporto i grafici in plots/...")
PlotNPDF("plots/GAM.pdf",          c(PlotGAMSplinesFullScan, PlotGAMLoessFullScan))
PlotPDF("plots/PPR.pdf",           PlotPPRScan)
PlotPDF("plots/neuralnetwork.pdf", PlotNeuralNetworkScan)
PlotNPDF("plots/CART.pdf",         c(PlotCARTPruned, PlotCARTScan))

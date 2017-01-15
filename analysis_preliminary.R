#-----------------------------------------------------------------------
# Analysis of the bike sharing dataset - Preliminary analysis
# Data Mining - 2015
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
# Obtained from: https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#-----------------------------------------------------------------------
if (!exists('analysis_preliminary.R')) { analysis_preliminary.R <- T
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Loads libraries and imports local scripts
source('R/squared_error.R')
source('R/plot_functions.R')
source('R/plot_pdf.R')
source('analysis_kmeans.R')


#-----------------------------------------------------------------------
# Configuration section
# Size of the training set (percentage)
kTrainingSetSize <- 0.75

# Size of the building set
kBuildingSetSize <- 0.75


#-----------------------------------------------------------------------
# Loads data and sets seed
bike.sharing <- read.table("data/hour.csv", header=T, sep=",")
set.seed(1076297)


#-----------------------------------------------------------------------
# Assigns better names to the variables
names(bike.sharing)[1]  <- "ID"
names(bike.sharing)[2]  <- "date"
names(bike.sharing)[4]  <- "year"
names(bike.sharing)[5]  <- "month"
names(bike.sharing)[6]  <- "hour"
names(bike.sharing)[7]  <- "isHoliday"
names(bike.sharing)[9]  <- "isWorkingday"
names(bike.sharing)[10] <- "weather"
names(bike.sharing)[11] <- "temperature"
names(bike.sharing)[12] <- "feelingTemp"
names(bike.sharing)[13] <- "humidity"
names(bike.sharing)[17] <- "total"


#-----------------------------------------------------------------------
# Converts qualitative variables which were stored as quantitative back
# to qualitative
bike.sharing$date         <- as.Date(bike.sharing$date)
bike.sharing$season       <- factor(bike.sharing$season)
bike.sharing$year         <- factor(bike.sharing$year)
bike.sharing$month        <- factor(bike.sharing$month)
bike.sharing$hour         <- factor(bike.sharing$hour)
bike.sharing$isHoliday    <- factor(bike.sharing$isHoliday)
bike.sharing$weekday      <- factor(bike.sharing$weekday)
bike.sharing$isWorkingday <- factor(bike.sharing$isWorkingday)
bike.sharing$weather      <- factor(bike.sharing$weather)


#-----------------------------------------------------------------------
# Merges weather == 3 and weather == 4
bike.sharing$weather[bike.sharing$weather == 4] <- 3
bike.sharing$weather <- factor(bike.sharing$weather)


#-----------------------------------------------------------------------
# Denormalizes data
# Coefficients for denormalizations are obtained from Readme file
# attached to the dataset
bike.sharing$temperature <- bike.sharing$temperature * 41.0
bike.sharing$feelingTemp <- bike.sharing$feelingTemp * 50.0
bike.sharing$humidity    <- bike.sharing$humidity    * 100.0
bike.sharing$windspeed   <- bike.sharing$windspeed   * 67.0


#-----------------------------------------------------------------------
# Enriches the bike.sharing object with methods
bike.sharing.rows <- function() { NROW(bike.sharing) }
bike.sharing.cols <- function() { NCOL(bike.sharing) }
bike.sharing.randomSampleIdx <- function(percentage = kTrainingSetSize) {
    sample(1:bike.sharing.rows(), bike.sharing.rows() * percentage)
}
bike.sharing.randomSample <- function(percentage = TS_size) {
    bike.sharing[bike.sharing.randomSampleIdx(percentage), ]
}


#-----------------------------------------------------------------------
# Aggregates hours into time slots
bike.sharing$timeslot <- bike.sharing.aggregateTimeslots(6)


#-----------------------------------------------------------------------
# Exports plots
PlotNPDF("plots/temperature-humidity.pdf", c(PlotFeelingVsTempByDay, PlotHumidityByDay))
PlotNPDF("plots/month-season.pdf", c(PlotTotalVsMonth, PlotTotalVsSeason))
PlotNPDF("plots/hour-timeslot.pdf", c(PlotTotalVsHour, PlotTotalVsTimeslot))


#-----------------------------------------------------------------------
# Removes outliers
bike.sharing <- bike.sharing[bike.sharing$date != as.Date("2011-03-10"), ]
bike.sharing <- bike.sharing[bike.sharing$date != as.Date("2012-08-17"), ]
bike.sharing <- bike.sharing[bike.sharing$date != as.Date("2012-10-29"), ]


#-----------------------------------------------------------------------
# Drop columns which are not useful (ID, year) 
bike.sharing <- bike.sharing[ , -c(1, 4)]


#-----------------------------------------------------------------------
# Prepares training and verify sets
random.sample <- bike.sharing.randomSampleIdx()
training.set  <- bike.sharing[random.sample, ]
verify.set    <- bike.sharing[-random.sample, ]

random.sample <- sample(1:NROW(training.set), NROW(training.set) * kBuildingSetSize)
building.set  <- training.set[random.sample, ]
checking.set  <- training.set[-random.sample, ]

ComputeMeanSquaredError <- function(model, set = verify.set) {
  prediction <- predict(model, set)
  MeanSquaredError(set$total, prediction)
}
rm(random.sample)


#-----------------------------------------------------------------------
# Formulae used in the analysis
formula.full  <- total ~ . -date -feelingTemp -casual -registered
formula.short <- total ~ temperature + humidity + windspeed + weather + timeslot + weekday + season

} # endif
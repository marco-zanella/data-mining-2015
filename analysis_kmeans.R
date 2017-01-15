#-----------------------------------------------------------------------
# Analysis of the bike sharing dataset - kmeans
# Data Mining - 2015
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
# Obtained from: https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#-----------------------------------------------------------------------
if (!exists('analysis_kmeans.R')) { analysis_kmeans.R <- T
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Loads libraries and imports local scripts
source('analysis_preliminary.R')

#-----------------------------------------------------------------------
# Aggregates hours into time slots
bike.sharing.aggregateTimeslots <-
function (time.slots)
{
  observed.hour <- as.numeric(as.character(bike.sharing$hour))
  
  # Clustering function, returns a map [hour, timeslot]
  Cluster <- function(time.slots) {
    slots <- data.frame(0:23)
    
    users <- vector("integer", 24)
    for (i in 0:23) {
      users[i + 1] <- mean(bike.sharing$total[observed.hour == i])
    }
    
    slots        <- data.frame(slots, kmeans(users, time.slots)$cluster)
    names(slots) <- c("hour", "timeslot")
    slots
  }

  
  cluster       <- Cluster(time.slots)
  
  slots <- vector("integer", bike.sharing.rows())
  for (i in 1:bike.sharing.rows()) {
    slots[i] <- cluster[observed.hour[i] + 1, 2]
  }
  
  factor(slots)
}

} # endif
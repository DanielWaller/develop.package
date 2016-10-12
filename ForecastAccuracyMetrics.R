#---------------------------#
# FORECAST ACCURACY METRICS #
#---------------------------#

#### Root Mean Squared Error (RMSE) ####

RootMeanSquaredError <- function(ForecastVec,ObservedVec){
  m <- length(ForecastVec); n <- length(ObservedVec)
  if (m == n){
    Error <- ForecastVec - ObservedVec
    RMSE <- sqrt((sum(Error*Error))/n)
    return(RMSE)
  }
  else{
    print("Error: vectors not equal length")
  }
}

#### Mean Absolute Error (MAE) ####

MeanAbsoluteError <- function(ForecastVec,ObservedVec){
  m <- length(ForecastVec); n <- length(ObservedVec)
  if (m == n){
    Error <- abs(ForecastVec - ObservedVec)
    MAE <- (sum(Error))/n
    return(MAE)
  }
  else{
    print("Error: vectors not equal length")
  }
}

#### Mean Absolute Percentage Error ####

MeanAbsolutePercentageError <- function(ForecastVec,ObservedVec){
  m <- length(ForecastVec); n <- length(ObservedVec)
  if (m == n){
    Error <- abs((ForecastVec - ObservedVec)/ObservedVec)
    MAPE <- 100*(sum(Error))/n
    return(MAPE)
  }
  else{
    print("Error: vectors not equal length")
  }
}

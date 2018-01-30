best <- function(state, outcome)
{
  library(dplyr)
  ## Read hospital data
  outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  ## ColClasses is used to read data faster by mentioning it's type, such as character, numeric etc
  bestHosp <- NULL
  rate <- NULL
  
  ## Function for heart attack
  HeartAttack <- function(s, HospData)
  {
    HospData <- select(HospData, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    filtered <- HospData[HospData$State==s & HospData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    
    ## Sort by Hospital name
    sortedData <- arrange(filtered, Hospital.Name)
    
    ## Sort by Rate
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    sortedData <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    
    ## Return hospital name in that state with lowest 30-day death
    bestHosp <- sortedData
    
  }
  
  ## Function for heart failure
  HeartFailure <- function(s, HospData)
  {
    HospData <- select(HospData, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    filtered <- HospData[HospData$State==s & HospData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    
    ## Sort by Hospital name
    sortedData <- arrange(filtered, Hospital.Name)
    
    ## Sort by Rate
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    sortedData <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    
    ## Return hospital name in that state with lowest 30-day death
    bestHosp <- sortedData
    
  }
  
  ## Function for pneumonia
  Pneumonia <- function(s, HospData)
  {
    HospData <- select(HospData, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    filtered <- HospData[HospData$State==s & HospData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    
    ## Sort by Hospital name
    sortedData <- arrange(filtered, Hospital.Name)
    
    ## Sort by Rate
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    sortedData <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    
    ## Return hospital name in that state with lowest 30-day death
    bestHosp <- sortedData
  }
  
  ## Main
  ## Check that outcome is valid
  if (outcome == "heart attack")
  {
    ## Check that state is valid
    if (length(outcomeData[outcomeData$State == state,c("State")])>0)
    {
      bh <- HeartAttack(state, outcomeData)
      bestHosp <- bh[1,c("Hospital.Name")]
      rate <- bh[1,2]
    } 
    else
    {
      print(paste("Error in best(", state, ", ", outcome,") : invalid state", sep=""))
    }
    
  } 
  else if (outcome == "heart failure")
  {
    ## Check that state is valid
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bh <- HeartFailure(state, outcomeData)
      bestHosp <- bh[1,c("Hospital.Name")]
      rate <- bh[1,2]
      
  } 
  else
    {
      print(paste("Error in best(", state, ", ", outcome,") : invalid state", sep=""))
    }
  } 
  else if (outcome == "pneumonia")
  {
    ## Check that state is valid
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bh <- Pneumonia(state, outcomeData)
      bestHosp <- bh[1,c("Hospital.Name")]
      rate <- bh[1,2]
    } 
    else
    {
      print(paste("Error in best(", state, ", ", outcome,") : invalid state", sep=""))
    }
  } 
  else
  {
    print(paste("Error in best(", state, ", ", outcome,") : invalid outcome", sep=""))
  }
  bestHosp
}

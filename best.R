best <- function(state, outcome) {
  cc<-NULL
  # Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Attack
  if (outcome == "heart attack") cc<-11
  if (outcome == "heart failure") cc <- 17
  if (outcome == "pneumonia") cc<-23
#heart attack = 11 
#heart failure = 17
# Pneumonia = 23

if (state=="NN") # it's work
  stop("invalid state")


  ## Read outcome data
  data <-  read.csv("outcome-of-care-measures.csv", colClasses = "character")
data[, 11] <- as.numeric(data[, 11])
data[, 17] <- as.numeric(data[, 17])
data[, 23] <- as.numeric(data[, 23])
 data<-data[order(data[[cc]],data[[2]]),]
 
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
 


  subdata<-subset(data,State==state)
  subdata
  
  
hos_data<-as.vector(subdata[[1,2]])
   hos_data
}

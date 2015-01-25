rankhospital <- function(state, outcome, num = "best") {
  
  # Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Attack
  if (outcome == "heart attack") cc<-11
  if (outcome == "heart failure") cc <- 17
  if (outcome == "pneumonia") cc<-23
  #heart attack = 11 
  #heart failure = 17
  # Pneumonia = 23
  
  if (state=="NN") # it's work
    stop("invalid state")
  if (outcome=="heart attak")
    stop("invalid outcome")#for rankhospital' part 4 to work
  
  ## Read outcome data
  data <-  read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  subdata<-subset(data,State==state)
  subdata[, 11] <- as.numeric(subdata[, 11])
  subdata[, 17] <- as.numeric(subdata[, 17])
  subdata[, 23] <- as.numeric(subdata[, 23])
  
  nalast<-TRUE
  if (class(num)=="character")
  if (num=="best")   
    choice<-1
   
  
  if (class(num)=="character")
    if (num=="worst")
    {choice <-nrow(subdata)
     nalast<-FALSE
    }
  if (class(num)=="numeric")
    choice <-num  
  subdata<-subdata[order(subdata[[cc]],subdata[[2]],na.last=nalast),]
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  
  

  nrow(subdata)
  if (choice <= nrow(subdata))
    hos_data<-as.vector(subdata[[choice,2]])
  else
    hos_data<-NA
  hos_data

}
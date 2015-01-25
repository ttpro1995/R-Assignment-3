rankall <- function(outcome, num = "best") {
  if (outcome=="heart attak")
    stop("invalid outcome")#for rankhospital' part 4 to work
  data <-  read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  ## Read outcome data
  if (outcome == "heart attack") cc<-11
  if (outcome == "heart failure") cc <- 17
  if (outcome == "pneumonia") cc<-23
  
  nalast<-TRUE
  if (class(num)=="character")
  {if (num=="best")
    
    choice<-1
   
  }
  else
    if (num=="worst")
    {choice <-row
     nalast<-FALSE
    }
  else
    choice <-num
  data<-data[order(data[[cc]],data[[2]],na.last=nalast),]
  
  
  

  
  
  ## Check that state and outcome are valid
  if (outcome=="heart attak")
    stop("invalid outcome")#for rankhospital' part 4 to work
  
  
  ## For each state, find the hospital of the given rank
  
  sta<-as.vector(data[["State"]])#get all state
  sta<-unique(sta)#unique state
  sta<-sort(sta)
  table<-NULL#contain a list of hospital which is return value
  for (i in sta)
  {
    tmphosname<-rankhospitalshort(i,data,num)
    tmpsta<-i
    columnrow<-c(tmphosname,tmpsta)
   table<- rbind(table,columnrow)    
  }
    rownames(table)<-sta
    colnames(table)<-c("hospital","state")
  table
  ## Return a data frame with the hospital names and the
  table<-as.data.frame(table)
  table
  ## (abbreviated) state name
}



rankhospitalshort <- function(state,data, choice) {# get in the already sort data frame, return the hospital name
  
 

  

  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  
  
  subdata<-subset(data,State==state)
  subdata
  row <-nrow(subdata)
  if (choice <= row)
    hos_data<-as.vector(subdata[[choice,2]])
  else
    hos_data<-NA
  hos_data

}
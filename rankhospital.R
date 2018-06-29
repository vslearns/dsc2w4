rankhospital <- function(state, outcome, num="best") {
  outcomes <- read.csv("pa3-data/outcome-of-care-measures.csv", colClasses = "character")
  statewise <- split(outcomes, outcomes$State)
  
  reqState <- statewise[[state]]
  if (outcome == "heart attack") {
    oc <- reqState[,c(2, 11)]
    oc <- oc[oc[,2] != "Not Available",]
  } else if (outcome == "heart failure") {
    oc <- reqState[,c(2, 17)]
    oc <- oc[oc[,2] != "Not Available",]
  } else if (outcome == "pneumonia") {
    oc <- reqState[,c(2, 23)]
    oc <- oc[oc[,2] != "Not Available",]
  } else {
    stop("invalid outcome")
  }
  
  oc[,2] <- as.numeric(oc[,2])
  if (length(oc) == 0) stop("invalid state")
  oc <- oc[order(oc[,2], oc[,1]),]
  
  if (num=="best") oc[1,1]
  else if (num=="worst") oc[nrow(oc),1]
  else if (num >= nrow(oc)) NA
  else oc[num,1]
}
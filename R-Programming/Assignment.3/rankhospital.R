rankhospital <- function(state, outcome, num = "best") {
  
  selectCol = 0;
  ## Read outcome data
  data = read.csv(
    "outcome-of-care-measures.csv", 
    na.strings ="Not Available",
    stringsAsFactors = FALSE
  )
  
  ## Check that state and outcome are valid
  if (outcome == "heart attack") { selectCol = 11}
  else if (outcome == "heart failure") {selectCol = 17}
  else if (outcome == "pneumonia") {selectCol = 23}
  else {stop("invalid outcome")}
  
  
    if (is.element(state, data$State) == FALSE){stop("invalid state")}
  ## Return hospital name in that state with the given rank 30-day death rate
  subData = subset(data, State == state)
  subData = subData[
    order(
      subData[,selectCol], 
      xtfrm(subData$Hospital.Name),
      decreasing=F
    ) , ]
  
  
  
  if(num=="best"){return (best(state, outcome))}
  
  else if (num =="worst"){
    
    k = range(subData[,selectCol], na.rm =TRUE)[2]
    subData = subset(subData, subData[,selectCol] == k)
    return (subData[nrow(subData),]$Hospital.Name)
  }
  else if (num > dim(subData)[1]){ return ("NA")}
  
  return (subData[num,]$Hospital.Name)
}
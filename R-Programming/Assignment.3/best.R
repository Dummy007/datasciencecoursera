best = function(state, outcome) {
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
  
  ## Return hospital name in that state with lowest 30-day death
  subData = subset(data, State == state)
  best.rate= range(subData[,selectCol], na.rm = TRUE)[1]
  answerData = subset(subData, subData[,selectCol] == best.rate)
  return (sort(answerData$Hospital.Name))
 
}

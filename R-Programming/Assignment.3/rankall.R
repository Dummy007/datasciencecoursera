rankall <- function(outcome, num = "best") 
{
  selectCol = 0;
  ## Read outcome data
  data = read.csv(
    "outcome-of-care-measures.csv", 
    na.strings ="Not Available",
    stringsAsFactors = FALSE
    )
  
  ## Check that outcome are valid
  if (outcome == "heart attack") { selectCol = 11}
  else if (outcome == "heart failure") {selectCol = 17}
  else if (outcome == "pneumonia") {selectCol = 23}
  else {stop("invalid outcome")}
  
 
  ## For each state, find the hospital of the given rank (abbreviated) state name

  the.states=character()
  the.names = character()
  for(the.state in unique(data$State))
  {
    subData = subset(data, State == the.state)
    subData = subData[ 
                      order(
                             subData[,selectCol], 
                             xtfrm(subData$Hospital.Name),
                             decreasing=F
                            ) ,
                      ]
    
    
    
    if(num=="best"){
      
      the.name = subData[1,]$Hospital.Name
      the.names = c(the.names, the.name)
      the.states = c(the.states, the.state)
    
    }
    
    
    
    else if (num =="worst"){
      
      k = range(subData[,selectCol], na.rm =TRUE)[2]
      subData = subset(subData, subData[,selectCol] == k)
      the.name = subData[nrow(subData),]$Hospital.Name
      the.names = c(the.names, the.name)
      the.states = c(the.states, the.state)
    }
    
    
    
    else if (num > dim(subData)[2]){ 
      return ("NA")
    }
    
    
    
    
    else{
      
      the.name = subData[num,]$Hospital.Name
      the.names = c(the.names, the.name)
      the.states = c(the.states, the.state)
    }
    
    
  }
    
  
   answer = data.frame(the.names, the.states, row.names = NULL) 
   names(answer) = c("hospital", "state")
  
  
   answer = answer[order(xtfrm(answer$state), decreasing = F),]
   rows = answer$state
   row.names(answer) = rows
  return (answer)
    
    
}






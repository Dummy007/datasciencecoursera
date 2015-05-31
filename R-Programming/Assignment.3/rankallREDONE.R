rankallREDONE <- function(outcome, num = "best") 
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
  
  #the.states=character()
  #the.names = character()
  
  subfunction = function (Data) {
    Data = Data[ 
      order(
      Data[,selectCol], 
      xtfrm(Data$Hospital.Name),
      decreasing=F
    ) , 
    ]
    
    if(num=="best"){
    
    the.name = Data[1,]$Hospital.Name
    return ( the.name )
    }
    
    else if (num =="worst"){
    
    k = range(Data[,selectCol], na.rm =TRUE)[2]
    Data = subset(Data, Data[,selectCol] == k)
    the.name = Data[nrow(Data),]$Hospital.Name
    return ( the.name )
    
    }
    
    else if (num > dim(Data)[2]){ 
    return ("NA")
    }
    
    else{
    
    the.name = Data[num,]$Hospital.Name
  
    
    return ( the.name )
    }
    
   
    
  }
  
  final = sapply(split(data,data$State), subfunction)
  
  
  
  return (final)
}

## BOWLERS generic function for host-wise data
# Please run the below line after downloading Ballers.RData file from Data folder
# load("Ballers.RData")

bowl.host <- function(number){
  # cols is a vector containing column indexes that we need for analysis for nth colsed bowler
  cols <- c(1, 3*number-1, 3*number, 3*number+1)
  
  #BOWLERS is the list of tibbles (available in Main sub-folder of Data folder)
  bowl.host <- BALLERS[3]$Host[cols] # host teams
  cat("Stats of", BALLERS$Name[number] ,"host-team-wise \n")
  print(bowl.host)
  
  strong.teams <- c("Australia", "India", "New Zealand", "South Africa", "England")
  weak.teams <- c("Ireland", "Bangladesh", "Afghanistan", "Zimbabwe")
  
  j <- 1
  
  strong.wkts <- 0
  weak.wkts <- 0
  strong.runs <- 0
  weak.runs <- 0
  
  for(i in 1:nrow(bowl.host)){
    if(bowl.host[[i, 1]] %in% strong.teams){
      # Only proceed if the data is not missing (i.e., not NA)
      if(!is.na(bowl.host[[i, 4]])){
        # Check for NA in Avg column
        cat("Avg of", BALLERS$Name[number] ,"in Test matches hosted in", bowl.host[[i, 1]], "is", bowl.host[[i, 4]], "\n")
      }
      
      if(!is.na(bowl.host[[i, 2]])){
        # Check for NA in Runs column
        strong.runs <- strong.runs + bowl.host[[i, 2]]
      }
      
      if(!is.na(bowl.host[[i, 3]])){
        # Check for NA in Wickets column
        strong.wkts <- strong.wkts + bowl.host[[i, 3]]
      }
      
      j <- j + 1
    }
  }
  #cat("\n")
  
  j <- 1
  #print(strong.wkts)
  
  # Avoid division by zero and handle NA
  if(!is.na(strong.wkts) && strong.wkts > 0){
    strong.avg <- strong.runs / strong.wkts
    cat("Overall average of", BALLERS$Name[number] ,"in tests hosted by strong teams is", strong.avg, "\n")
  } else {
    cat("No wickets taken or insufficient data in strong test playing hosts \n")
    strong.avg = NA
  }
  
  cat("\n")
  
  
  for(i in 1:nrow(bowl.host)){
    if(bowl.host[[i, 1]] %in% weak.teams){
      # Only proceed if the data is not missing (i.e., not NA)
      if(!is.na(bowl.host[[i, 4]])){ # Check for NA in Avg column
        cat("Avg of", BALLERS$Name[number] ,"in Test matches hosted in", bowl.host[[i, 1]], "is", bowl.host[[i, 4]], "\n")
      }
      
      if(!is.na(bowl.host[[i, 2]])){ # Check for NA in Runs column
        weak.runs <- weak.runs + bowl.host[[i, 2]]
      }
      
      if(!is.na(bowl.host[[i, 3]])){ # Check for NA in Wickets column
        weak.wkts <- weak.wkts + bowl.host[[i, 3]]
      }
      
      j <- j + 1
    }
  }
  
  #print(weak.wkts)
  
  # Avoid division by zero and handle NA
  # Useful for BALLERS like Josh Hazlewood who haven't played against weak teams
  if(!is.na(weak.wkts) && weak.wkts > 0){
    weak.avg <- weak.runs / weak.wkts
    cat("Overall average of", BALLERS$Name[number] ,"in tests hosted by weak teams is", weak.avg, "\n")
  } else {
    cat("No wickets taken or insufficient data in weak test playing hosts \n")
    weak.avg = NA
  }
  return(c(strong.wkts, weak.wkts, strong.runs, weak.runs, strong.avg, weak.avg))
}

load("../../Data/Ballers.RData")

SW = numeric(10)
WW = numeric(10)
SR = numeric(10)
WR = numeric(10)
SA = numeric(10)
WA = numeric(10)

for(i in 1:10)
{
  out = bowl.host(i)
  SW[i] = out[1]
  WW[i] = out[2]
  SR[i] = out[3]
  WR[i] = out[4]
  SA[i] = out[5]
  WA[i] = out[6]
}

df = data.frame(SW, WW, SR, WR, SA, WA, row.names =  BALLERS$Name)
colnames(df) = c("St. Wkts", "W. Wkts", "Str. Runs", "W. Runs", "St. Avg", "W. Avg")
Ball_host = df
Ball_host

save(Ball_host, file = "../Ball_host.RData")

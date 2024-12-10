## All-roundersbowling generic function for host-wise data
# Kindly run the below line after downloading "Allrounders_ball.RData" from the Data folder.
#load("Allrounders_ball.RData")

allround.bowl.host <- function(number){
  # cols is a vector containing column indexes that we need for analysis for nth colsed bowler
  cols <- c(1, 3*number-1, 3*number, 3*number+1)
  
  #BOWLERS is the list of tibbles (available in Main sub-folder of Data folder)
  allround.bowl.host <- ARS_ball[3]$Host[cols] # host teams
  cat("Stats of", ARS_ball$Name[number] ,"host-team-wise \n")
  print(allround.bowl.host)
  
  strong.teams <- c("Australia", "India", "New Zealand", "South Africa", "England")
  weak.teams <- c("Ireland", "Bangladesh", "Afghanistan", "Zimbabwe")
  
  j <- 1
  
  strong.wkts <- 0
  weak.wkts <- 0
  strong.runs <- 0
  weak.runs <- 0
  
  for(i in 1:nrow(allround.bowl.host)){
    if(allround.bowl.host[[i, 1]] %in% strong.teams){
      # Only proceed if the data is not missing (i.e., not NA)
      if(!is.na(allround.bowl.host[[i, 4]])){
        # Check for NA in Avg column
        cat("Avg of", ARS_ball$Name[number] ,"in Test matches hosted in", allround.bowl.host[[i, 1]], "is", allround.bowl.host[[i, 4]], "\n")
      }
      
      if(!is.na(allround.bowl.host[[i, 2]])){
        # Check for NA in Runs column
        strong.runs <- strong.runs + allround.bowl.host[[i, 2]]
      }
      
      if(!is.na(allround.bowl.host[[i, 3]])){
        # Check for NA in Wickets column
        strong.wkts <- strong.wkts + allround.bowl.host[[i, 3]]
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
    cat("Overall average of", ARS_ball$Name[number] ,"in tests hosted by strong teams is", strong.avg, "\n")
  } else {
    cat("No wickets taken or insufficient data in strong test playing hosts \n")
    strong.avg = NA
  }
  
  cat("\n")
  
  
  for(i in 1:nrow(allround.bowl.host)){
    if(allround.bowl.host[[i, 1]] %in% weak.teams){
      # Only proceed if the data is not missing (i.e., not NA)
      if(!is.na(allround.bowl.host[[i, 4]])){ # Check for NA in Avg column
        cat("Avg of", ARS_ball$Name[number] ,"in Test matches hosted in", allround.bowl.host[[i, 1]], "is", allround.bowl.host[[i, 4]], "\n")
      }
      
      if(!is.na(allround.bowl.host[[i, 2]])){ # Check for NA in Runs column
        weak.runs <- weak.runs + allround.bowl.host[[i, 2]]
      }
      
      if(!is.na(allround.bowl.host[[i, 3]])){ # Check for NA in Wickets column
        weak.wkts <- weak.wkts + allround.bowl.host[[i, 3]]
      }
      
      j <- j + 1
    }
  }
  
  #print(weak.wkts)
  
  # Avoid division by zero and handle NA
  # Useful for ARS_ball like Josh Hazlewood who haven't played against weak teams
  if(!is.na(weak.wkts) && weak.wkts > 0){
    weak.avg <- weak.runs / weak.wkts
    cat("Overall average of", ARS_ball$Name[number] ,"in tests hosted by weak teams is", weak.avg, "\n")
  } else {
    cat("No wickets taken or insufficient data in weak test playing hosts \n")
    weak.avg = NA
  }
  return(c(strong.wkts, weak.wkts, strong.runs, weak.runs, strong.avg, weak.avg))
}

load("../../Data/Allrounders_ball.RData")

SW = numeric(10)
WW = numeric(10)
SR = numeric(10)
WR = numeric(10)
SA = numeric(10)
WA = numeric(10)

for(i in 1:10)
{
  out = allround.bowl.host(i)
  SW[i] = out[1]
  WW[i] = out[2]
  SR[i] = out[3]
  WR[i] = out[4]
  SA[i] = out[5]
  WA[i] = out[6]
}

df = data.frame(SW, WW, SR, WR, SA, WA, row.names =  ARS_ball$Name)
colnames(df) = c("St. Wkts", "W. Wkts", "Str. Runs", "W. Runs", "St. Avg", "W. Avg")
ARS_ball_host = df
ARS_ball_host

save(ARS_ball_host, file = "../Allround_ball_host.RData")

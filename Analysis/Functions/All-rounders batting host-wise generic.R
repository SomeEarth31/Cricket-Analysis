## allrounder batting host wise 
# Please load Allrounders_bat.RData file by running the below commented code
# load("Allrounders_bat.RData")
# You can download this file from the Main sub-folder of Data Folder

allround.bat.host <- function(number){
  # cols is a vector containing column indexes that we need for analysis for nth colsed bowler
  cols <- c(1, 3*number-1, 3*number, 3*number+1)
  
  #BOWLERS is the list of tibbles (available in Main sub-folder of Data folder)
  allround.bat.host <- ARS_bat[3]$Host[cols] # host teams
  cat("Stats of", ARS_bat$Name[number] ,"host-team-wise \n")
  print(allround.bat.host)
  
  strong.teams <- c("Australia", "India", "New Zealand", "South Africa", "England")
  weak.teams <- c("Ireland", "Bangladesh", "Afghanistan", "Zimbabwe")
  
  j <- 1
  
  strong.runs <- 0
  weak.runs <- 0
  strong.inns <- 0
  weak.inns <- 0
  
  for(i in 1:nrow(allround.bat.host)){
    # Convert columns to numeric, safely handling NAs and non-numeric values
    avg_value <- suppressWarnings(as.numeric(allround.bat.host[[i, 4]]))
    inns_value <- suppressWarnings(as.numeric(allround.bat.host[[i, 2]]))
    no_value <- suppressWarnings(as.numeric(allround.bat.host[[i, 3]]))
    
    
    if(allround.bat.host[[i, 1]] %in% strong.teams){
      # Only proceed if the data is not missing (i.e., not NA)
      if(!is.na(allround.bat.host[[i, 4]])){
        # Check for NA in Avg column
        cat("Avg of", ARS_bat$Name[number] ,"in Test matches hosted in", allround.bat.host[[i, 1]], "is", allround.bat.host[[i, 4]], "\n")
      }
      
      if(!is.na(allround.bat.host[[i, 2]])){
        # Check for NA in Runs column
        strong.runs <- strong.runs + avg_value * (inns_value - no_value)
      }
      
      if(!is.na(allround.bat.host[[i, 3]])){
        # Check for NA in Wickets column
        strong.inns <- strong.inns + (inns_value - no_value)
      }
      
      j <- j + 1
    }
  }
  
  j <- 1

  
  # Avoid division by zero and handle NA
  if(!is.na(strong.inns) && strong.inns > 0){
    strong.avg <- strong.runs / strong.inns
    cat("Overall average of", ARS_bat$Name[number] ,"in tests hosted by strong teams is", strong.avg, "\n")
  } else {
    cat("No wickets taken or insufficient data in strong test playing hosts \n")
    strong.avg = NA
  }
  
  cat("\n")
  
  
  for(i in 1:nrow(allround.bat.host)){
    avg_value <- suppressWarnings(as.numeric(allround.bat.host[[i, 4]]))
    inns_value <- suppressWarnings(as.numeric(allround.bat.host[[i, 2]]))
    no_value <- suppressWarnings(as.numeric(allround.bat.host[[i, 3]]))
    
    if(allround.bat.host[[i, 1]] %in% weak.teams){
      # Only proceed if the data is not missing (i.e., not NA)
      if(!is.na(allround.bat.host[[i, 4]])){ # Check for NA in Avg column
        cat("Avg of", ARS_bat$Name[number] ,"in Test matches hosted in", allround.bat.host[[i, 1]], "is", allround.bat.host[[i, 4]], "\n")
      }
      
      if(!is.na(allround.bat.host[[i, 2]])){ # Check for NA in Runs column
        weak.runs <- weak.runs + avg_value * (inns_value - no_value)
      }
      
      if(!is.na(allround.bat.host[[i, 3]])){ # Check for NA in Wickets column
        weak.inns <- weak.inns + (inns_value - no_value)
      }
      
      j <- j + 1
    }
  }
  
  #print(weak.inns)
  
  # Avoid division by zero and handle NA
  # Useful for ARS_bat_ball like Josh Hazlewood who haven't played against weak teams
  if(!is.na(weak.inns) && weak.inns > 0){
    weak.avg <- weak.runs / weak.inns
    cat("Overall average of", ARS_bat$Name[number] ,"in tests hosted by weak teams is", weak.avg, "\n")
  } else {
    cat("No wickets taken or insufficient data in weak test playing hosts \n")
    weak.avg = NA
  }
  return(c(strong.runs, weak.runs, strong.inns, weak.inns,strong.avg, weak.avg))
}


load("../../Data/Allrounders_bat.RData")


SR = numeric(10)
WR = numeric(10)
SI = numeric(10)
WI = numeric(10)
SA = numeric(10)
WA = numeric(10)

for(i in 1:10)
{
  out = allround.bat.host(i)
  SR[i] = out[1]
  WR[i] = out[2]
  SI[i] = out[3]
  WI[i] = out[4]
  SA[i] = out[5]
  WA[i] = out[6]
}

df = data.frame(SR, WR, SI, WI, SA, WA, row.names = ARS_bat$Name)
colnames(df) = c("St. Runs", "W. Runs", "Str. Inn", "W. Inn", "St. Avg", "W. Avg")
ARS_bat_host = df
ARS_bat_host

save(ARS_bat_host, file = "../Allround_bat_host.RData")

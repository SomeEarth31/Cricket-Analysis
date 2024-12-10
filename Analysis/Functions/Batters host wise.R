# generic function of batters host-wise stats
# Please load the BATTERS.RData file first by uncommenting the below code
# load("BATTERS.RData")
# You can find this file in the Main sub-folder within Data folder


bat.host <- function(number){
  # cols is a vector containing column indexes that we need for analysis for nth colsed batter
  cols <- c(1, 3*number-1, 3*number, 3*number+1)
  
  # BATTERS is the list of tibbles (available in Main sub-folder of Data folder)
  bat.host <- BATTERS[3]$Host[cols] # host teams
  
  cat("Stats of", BATTERS$Name[number], "host-wise \n")
  print(bat.host)
  
  strong.teams <- c("Australia", "India", "New Zealand", "South Africa", "England")
  weak.teams <- c("Ireland", "Bangladesh", "Afghanistan", "Zimbabwe")
  
  strong.runs <- 0
  weak.runs <- 0
  strong.inns <- 0
  weak.inns <- 0
  
  # Loop through host teams and calculate stats
  for(i in 1:nrow(bat.host)){
    # Convert columns to numeric, safely handling NAs and non-numeric values
    # We have to do this because of Harry Brooks, who never got out against weak teams so there's a non-NA, non-numeric value
    avg_value <- suppressWarnings(as.numeric(bat.host[[i, 4]]))
    inns_value <- suppressWarnings(as.numeric(bat.host[[i, 2]]))
    no_value <- suppressWarnings(as.numeric(bat.host[[i, 3]]))
    
    if(bat.host[[i, 1]] %in% strong.teams){
      # Only proceed if avg is not NA
      if(!is.na(avg_value)){
        cat("Avg of", BATTERS$Name[number], "in Test matches hosted in", bat.host[[i, 1]], "is", avg_value, "\n")
        
        # If all values are numeric, calculate strong runs and innings
        if(!is.na(inns_value) & !is.na(no_value) & !is.na(avg_value)){
          strong.runs <- strong.runs + avg_value * (inns_value - no_value)
        }
        if(!is.na(inns_value) & !is.na(no_value)){
          strong.inns <- strong.inns + (inns_value - no_value)
        }
      }
    }
  }
  
  # Avoid division by zero and handle NA
  if(!is.na(strong.inns) && strong.inns > 0){
    strong.avg <- strong.runs / strong.inns
    cat("Overall average of", BATTERS$Name[number], "in tests hosted by strong teams is", strong.avg, "\n")
  } else {
    cat("Has not batted or insufficient data in tests hosted strong teams\n")
    strong.avg = NA
  }
  
  cat("\n")
  
  for(i in 1:nrow(bat.host)){
    avg_value <- suppressWarnings(as.numeric(bat.host[[i, 4]]))
    inns_value <- suppressWarnings(as.numeric(bat.host[[i, 2]]))
    no_value <- suppressWarnings(as.numeric(bat.host[[i, 3]]))
    
    if(bat.host[[i, 1]] %in% weak.teams){
      if(!is.na(avg_value)){
        cat("Avg of", BATTERS$Name[number], "in Test matches hosted in", bat.host[[i, 1]], "is", avg_value, "\n")
        
        # If all values are numeric, calculate weak runs and innings
        if(!is.na(inns_value) & !is.na(no_value) & !is.na(avg_value)){
          weak.runs <- weak.runs + avg_value * (inns_value - no_value)
        }
        if(!is.na(inns_value) & !is.na(no_value)){
          weak.inns <- weak.inns + (inns_value - no_value)
        }
      }
    }
  }
  
  # Avoid division by zero and handle NA
  if(!is.na(weak.inns) && weak.inns > 0){
    weak.avg <- weak.runs / weak.inns
    cat("Overall average of", BATTERS$Name[number], "in tests hosted by weak teams is", weak.avg, "\n")
  } else {
    cat(BATTERS$Name[number], "has not batted or insufficient data in tests hosted weak teams\n")
    weak.avg = NA
  }
  return(c(strong.runs, weak.runs, strong.inns, weak.inns,strong.avg, weak.avg))
}

load("../../Data/Batters.RData")


SR = numeric(10)
WR = numeric(10)
SI = numeric(10)
WI = numeric(10)
SA = numeric(10)
WA = numeric(10)

for(i in 1:10)
{
  out = bat.host(i)
  SR[i] = out[1]
  WR[i] = out[2]
  SI[i] = out[3]
  WI[i] = out[4]
  SA[i] = out[5]
  WA[i] = out[6]
}

df = data.frame(SR, WR, SI, WI, SA, WA, row.names = BATTERS$Name)
colnames(df) = c("St. Runs", "W. Runs", "Str. Inn", "W. Inn", "St. Avg", "W. Avg")
Bat_host = df
Bat_host

save(Bat_host, file = "../Bat_host.RData")
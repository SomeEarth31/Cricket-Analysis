

allround.bat.against <- function(number){
  # rank is a vector containing column indexes that we need for analysis for nth ranked batter
  rank <- c(1, 3*number-1, 3*number, 3*number+1)
  
  # ARS_bat is the list of tibbles (available in Main sub-folder of Data folder)
  allround.bat.opposition <- ARS_bat[2]$Against[rank] # opposition teams
  
  cat("Stats of", ARS_bat$Name[number], "opposition-wise \n")
  print(allround.bat.opposition)
  
  strong.teams <- c("Australia", "India", "New Zealand", "South Africa", "England")
  weak.teams <- c("Ireland", "Bangladesh", "Afghanistan", "Zimbabwe")
  
  strong.runs <- 0
  weak.runs <- 0
  strong.inns <- 0
  weak.inns <- 0
  
  # Loop through opposition teams and calculate stats
  for(i in 1:nrow(allround.bat.opposition)){
    # Convert columns to numeric, safely handling NAs and non-numeric values
    # We have to do this because of Harry Brooks, who never got out against weak teams so there's a non-NA, non-numeric value
    avg_value <- suppressWarnings(as.numeric(allround.bat.opposition[[i, 4]]))
    inns_value <- suppressWarnings(as.numeric(allround.bat.opposition[[i, 2]]))
    no_value <- suppressWarnings(as.numeric(allround.bat.opposition[[i, 3]]))
    
    if(allround.bat.opposition[[i, 1]] %in% strong.teams){
      # Only proceed if avg is not NA
      if(!is.na(avg_value)){
        cat("Avg of", ARS_bat$Name[number], "against", allround.bat.opposition[[i, 1]], "is", avg_value, "\n")
        
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
    cat("Overall average of", ARS_bat$Name[number], "against strong teams is", strong.avg, "\n")
  } else {
    cat("Has not batted or insufficient data against strong teams\n")
    strong.avg = NA
  }
  
  cat("\n")
  
  for(i in 1:nrow(allround.bat.opposition)){
    avg_value <- suppressWarnings(as.numeric(allround.bat.opposition[[i, 4]]))
    inns_value <- suppressWarnings(as.numeric(allround.bat.opposition[[i, 2]]))
    no_value <- suppressWarnings(as.numeric(allround.bat.opposition[[i, 3]]))
    
    if(allround.bat.opposition[[i, 1]] %in% weak.teams){
      if(!is.na(avg_value)){
        cat("Avg of", ARS_bat$Name[number], "against", allround.bat.opposition[[i, 1]], "is", avg_value, "\n")
        
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
    cat("Overall average of", ARS_bat$Name[number], "against weak teams is", weak.avg, "\n")
  } else {
    cat(ARS_bat$Name[number], "has not batted or insufficient data against weak teams\n")
    weak.avg = NA
  }
  return(c(strong.runs, weak.runs,strong.inns, weak.inns,strong.avg, weak.avg))
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
  out = allround.bat.against(i)
  SR[i] = out[1]
  WR[i] = out[2]
  SI[i] = out[3]
  WI[i] = out[4]
  SA[i] = out[5]
  WA[i] = out[6]
}

df = data.frame(SR, WR, SI, WI, SA, WA, row.names = ARS_bat$Name)
colnames(df) = c("St. Runs", "W. Runs", "Str. Inn", "W. Inn", "St. Avg", "W. Avg")
ARS_bat_opp = df
ARS_bat_opp

save(ARS_bat_opp, file = "../Allround_bat_opp.RData")

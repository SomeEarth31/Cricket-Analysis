# Please load the below RData file before using this function.
# load("Allrounders_ball.RData")

allround.bowl.against <- function(cols){
  # rank is a vector containing column indexes that we need for analysis for nth ranked bowler
  rank <- c(1, 3*cols-1, 3*cols, 3*cols+1)
  
  #ARS_ball is the list of tibbles (available in Main sub-folder of Data folder)
  allround.bowl.opposition <- ARS_ball[2]$Against[rank] # opposition teams
  cat("Stats of", ARS_ball$Name[cols] ,"opposition-wise \n")
  print(allround.bowl.opposition)
  
  strong.teams <- c("Australia", "India", "New Zealand", "South Africa", "England")
  weak.teams <- c("Ireland", "Bangladesh", "Afghanistan", "Zimbabwe")
  
  j <- 1
  
  strong.wkts <- 0
  weak.wkts <- 0
  strong.runs <- 0
  weak.runs <- 0
  
  for(i in 1:nrow(allround.bowl.opposition)){
    avg_value <- suppressWarnings(as.numeric(allround.bowl.opposition[[i, 4]]))
    runs_value <- suppressWarnings(as.numeric(allround.bowl.opposition[[i, 2]]))
    wkts_value <- suppressWarnings(as.numeric(allround.bowl.opposition[[i, 3]]))
    
    if(allround.bowl.opposition[[i, 1]] %in% strong.teams){
      # Only proceed if the data is not missing (i.e., not NA)
      if(!is.na(avg_value)){
        # Check for NA in Avg column
        cat("Avg of", ARS_ball$Name[cols] ,"against", allround.bowl.opposition[[i, 1]], "is", avg_value, "\n")
      }
      
      if(!is.na(runs_value)){
        # Check for NA in Runs column
        strong.runs <- strong.runs + runs_value
      }
      
      if(!is.na(wkts_value)){
        # Check for NA in Wickets column
        strong.wkts <- strong.wkts + wkts_value
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
    cat("Overall average of", ARS_ball$Name[cols] ,"against strong teams is", strong.avg, "\n")
  } else {
    cat("No wickets taken or insufficient data against strong teams\n")
    strong.avg = NA
  }
  
  cat("\n")
  
  
  for(i in 1:nrow(allround.bowl.opposition)){
    # We need to do this because 9th ranked all-rounder Ben Stokes has bowled
    # against Ireland, but he has not taken any wickets.
    avg_value <- suppressWarnings(as.numeric(allround.bowl.opposition[[i, 4]]))
    runs_value <- suppressWarnings(as.numeric(allround.bowl.opposition[[i, 2]]))
    wkts_value <- suppressWarnings(as.numeric(allround.bowl.opposition[[i, 3]]))
    
    if(allround.bowl.opposition[[i, 1]] %in% weak.teams){
      # Only proceed if the data is not missing (i.e., not NA)
      if(!is.na(avg_value)){ # Check for NA in Avg column
        cat("Avg of", ARS_ball$Name[cols] ,"against", allround.bowl.opposition[[i, 1]], "is", avg_value, "\n")
      }
      
      if(!is.na(runs_value)){ # Check for NA in Runs column
        weak.runs <- weak.runs + runs_value
      }
      
      if(!is.na(wkts_value)){ # Check for NA in Wickets column
        weak.wkts <- weak.wkts + wkts_value
      }
      
      j <- j + 1
    }
  }
  
  #print(weak.wkts)
  
  # Avoid division by zero and handle NA
  # Useful for ARS_ball like Josh Hazlewood who haven't played against weak teams
  if(!is.na(weak.wkts) && weak.wkts > 0){
    weak.avg <- weak.runs / weak.wkts
    cat("Overall average of", ARS_ball$Name[cols] ,"against weak teams is", weak.avg, "\n")
  } else {
    cat("No wickets taken or insufficient data against weak teams\n")
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
  out = allround.bowl.against(i)
  SW[i] = out[1]
  WW[i] = out[2]
  SR[i] = out[3]
  WR[i] = out[4]
  SA[i] = out[5]
  WA[i] = out[6]
}

df = data.frame(SW, WW, SR, WR, SA, WA, row.names =  ARS_ball$Name)
colnames(df) = c("St. Wkts", "W. Wkts", "Str. Runs", "W. Runs", "St. Avg", "W. Avg")
ARS_ball_op = df
ARS_ball_op

save(ARS_ball_op, file = "../Allround_ball_opp.RData")

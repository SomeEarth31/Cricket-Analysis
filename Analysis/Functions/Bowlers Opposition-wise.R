# Pardon incorrect naming of variables (cols, rank instead of n, cols resp.)
# cols is the rank no. that we are looking for

bowl.against <- function(cols){
  # rank is a vector containing column indexes that we need for analysis for nth ranked bowler
  rank <- c(1, 3*cols-1, 3*cols, 3*cols+1)

  #BALLERS is the list of tibbles (available in Main sub-folder of Data folder)
  ash.opposition <- BALLERS[2]$Against[rank] # opposition teams
  cat("Stats of", BALLERS$Name[cols] ,"opposition-wise \n")
  print(ash.opposition)
  
  strong.teams <- c("Australia", "India", "New Zealand", "South Africa", "England")
  weak.teams <- c("Ireland", "Bangladesh", "Afghanistan", "Zimbabwe")
  
  j <- 1

  strong.wkts <- 0
  weak.wkts <- 0
  strong.runs <- 0
  weak.runs <- 0
  
  for(i in 1:nrow(ash.opposition)){
    if(ash.opposition[[i, 1]] %in% strong.teams){
      # Only proceed if the data is not missing (i.e., not NA)
      if(!is.na(ash.opposition[[i, 4]])){
        # Check for NA in Avg column
        cat("Avg of", BALLERS$Name[cols] ,"against", ash.opposition[[i, 1]], "is", ash.opposition[[i, 4]], "\n")
      }

      if(!is.na(ash.opposition[[i, 2]])){
        # Check for NA in Runs column
        strong.runs <- strong.runs + ash.opposition[[i, 2]]
        }
        
      if(!is.na(ash.opposition[[i, 3]])){
        # Check for NA in Wickets column
        strong.wkts <- strong.wkts + ash.opposition[[i, 3]]
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
  cat("Overall average of", BALLERS$Name[cols] ,"against strong teams is", strong.avg, "\n")
} else {
  cat("No wickets taken or insufficient data against strong teams\n")
}

cat("\n")


for(i in 1:nrow(ash.opposition)){
  if(ash.opposition[[i, 1]] %in% weak.teams){
    # Only proceed if the data is not missing (i.e., not NA)
    if(!is.na(ash.opposition[[i, 4]])){ # Check for NA in Avg column
      cat("Avg of", BALLERS$Name[cols] ,"against", ash.opposition[[i, 1]], "is", ash.opposition[[i, 4]], "\n")
    }

    if(!is.na(ash.opposition[[i, 2]])){ # Check for NA in Runs column
      weak.runs <- weak.runs + ash.opposition[[i, 2]]
    }

    if(!is.na(ash.opposition[[i, 3]])){ # Check for NA in Wickets column
      weak.wkts <- weak.wkts + ash.opposition[[i, 3]]
    }

    j <- j + 1
  }
}

#print(weak.wkts)

# Avoid division by zero and handle NA
  # Useful for BALLERS like Josh Hazlewood who haven't played against weak teams
if(!is.na(weak.wkts) && weak.wkts > 0){
  weak.avg <- weak.runs / weak.wkts
  cat("Overall average of", BALLERS$Name[cols] ,"against weak teams is", weak.avg, "\n")
} else {
  cat("No wickets taken or insufficient data against weak teams\n")
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
  out = bowl.against(i)
  SW[i] = out[1]
  WW[i] = out[2]
  SR[i] = out[3]
  WR[i] = out[4]
  SA[i] = out[5]
  WA[i] = out[6]
}

df = data.frame(SW, WW, SR, WR, SA, WA, row.names =  BALLERS$Name)
colnames(df) = c("St. Wkts", "W. Wkts", "Str. Runs", "W. Runs", "St. Avg", "W. Avg")
Ball_opp = df
Ball_opp

save(Ball_opp, file = "../Ball_opp.RData")

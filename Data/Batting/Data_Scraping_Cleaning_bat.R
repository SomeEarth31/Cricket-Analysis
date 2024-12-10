library(tidyverse)
library(rvest)
library(dplyr)

#--------------------------------------------------------------------#

#Main website
html = read_html("https://www.espncricinfo.com/rankings/icc-player-ranking")
#tables = html %>% html_table()

#Getting the required links
links = html %>% html_elements("td[class='ds-min-w-max'] a") %>% html_attr("href")

batters = links[1:10]
#ballers = links[11:20]
#all_rounders = links[21:30]

#Formatting stuff
first = "https://www.espncricinfo.com"
last = "/bowling-batting-stats"
last2 = "/photos"

## To create playerwise links
# link = paste(first,batters[1],last, sep = "") -> just change array and index according keep everything else the same

# This function takes in two inputs, the link of the player's website 
clean_bat = function(web_link,ind,photo_link) 
{
  #Reading the link
  html_link = read_html(web_link)
  
  #Get the type of player
  p_type = html_link %>% html_elements("span[class='ds-text-comfortable-s']") %>% html_text()
  p_type = p_type[2]
  
  if(p_type == "Bowling Allrounder" | p_type == "Bowler")
  {
    #Call Python function here, that generated the RData file. 
    system(paste("conda run -n Webscrape python Syn_Scrape_bat.py", web_link, ind))
    load(paste(ind,".RData",sep=""))
    all_tables = tables
  }
  else
  {
    all_tables = html_link %>% html_table()
  }
  
  #Picutre
  html_pic = read_html(photo_link)
  pic_a = html_pic %>% html_elements("img") %>% html_attr("src")
  pic = grep("\\.jpg$", pic_a, value = TRUE)
  pic = pic[1]
  #pictures = html_link %>% html_elements("link[as='image']") %>% html_attr("href")
  #pic = pictures[1]
  
  #Getting the overall stats
  over_all = all_tables[[1]]$Avg
  
  #Getting the team wise stats and renaming the columns
  team_w = all_tables[[2]][c(1,4,5,8)] 
  team_w[1] = team_w[[1]] %>% substring(4)
  team_w[2] = team_w[[2]] %>% as.numeric()
  names(team_w)[1] = "Country"
  
  #Getting host country stats and renaming columns
  country_w = all_tables[[3]][c(1,4,5,8)] 
  country_w[1] = country_w[[1]] %>% substring(4)
  country_w[2] = country_w[[2]] %>% as.numeric()
  names(country_w)[1] = "Country"
  
  #Returning three tibles as lists
  return(list(OA = over_all, Teams = team_w, Country = country_w, Picture = pic))
  
}

#Array of all the teams played against, and all the host countries. This is to maintain unifromity.
TEAMS_C = c("Afghanistan", "Australia", "Bangladesh","England" , "India", "Ireland", "New Zealand",
            "Pakistan","South Africa", "Sri Lanka", "West Indies", "Zimbabwe")

TEAMS_H = c("Australia", "Bangladesh","England" , "India", "New Zealand",
            "Pakistan","South Africa", "Sri Lanka", "U.A.E.", "West Indies", "Zimbabwe")


# This function has one input, the array of player links(created above),
# This outputs three dataframes and one array of plahyer names. The dataframes are:
# 1. The player wise overall averages, 
# 2. The countries played against stats per player
# 3. The host countries stats per players


data_gen = function(players)
{
  merged_data_C = tibble(Country = TEAMS_C)
  merged_data_H = tibble(Country = TEAMS_H)
  merged_data_oa = tibble()
  names = c()
  pictures = c()
  i=0
  for(p in players)
  {
    i=i+1
    #Getting the names of the players
    namet = gsub("[0-9]","",p  %>% substring(13))
    namet = namet %>% strsplit("-")
    name = paste(paste(namet[[1]][1] %>% substring(1,1) %>% toupper(),namet[[1]][1] %>% substring(2), sep=""),
                 paste(namet[[1]][2] %>% substring(1,1) %>% toupper(),namet[[1]][2] %>% substring(2), sep=""))
    names = c(names, name)
    
    #Calling the clean function and renaming the collumns
    out_clean = clean_bat(paste(first,p,last, sep = ""),i,paste(first,p,last2, sep = ""))
    pictures = c(pictures, out_clean$Picture)
    
    names(out_clean$Teams)[2] = paste(name,"inns")
    names(out_clean$Teams)[3] = paste(name,"NO")
    names(out_clean$Teams)[4] = paste(name,"Avg")
    
    names(out_clean$Country)[2] = paste(name, "inns")
    names(out_clean$Country)[3] = paste(name, "NO")
    names(out_clean$Country)[4] = paste(name, "Avg")
    
    #Creating the merged tibbles for all the players
    OA = tibble(Name = name, OA = out_clean$OA)
    
    merged_data_C = full_join(merged_data_C, out_clean$Teams, by= "Country")
    merged_data_H = full_join(merged_data_H, out_clean$Country, by= "Country")
    merged_data_oa = bind_rows(merged_data_oa, OA)
  }
  
  #Returning the tibbles
  return(list(Name = names, Against = merged_data_C, Host = merged_data_H, OA = merged_data_oa, Picture = pictures ))
}



BATTERS = data_gen(batters)

save(BATTERS, file = "../Batters.RData")






#
# This is the server 2 logic. 


library(shiny)
library(tidyverse)
library(dplyr)

load("../Data/Ballers.RData")
load("../Data/Batters.RData")
load("../Data/Allrounders_ball.RData")
load("../Data/Allrounders_bat.RData")

#-----Some definitions required for the second pane-------#
player_batter_stats = BATTERS$OA

player_baller_stats = data.frame(
  Name = BALLERS$OA$Name,
  OA = abs(round(rnorm(10, 15, 60)))
)

extra_player_stats = data.frame(
  Name = c("Krati", "Soham", "Shreyasi", "Samarth"),
  OA = abs(round(rnorm(4, 15, 60)))
)
player_stats = rbind(player_batter_stats, player_baller_stats, extra_player_stats)

# Define the server logic
server2 <- function(input, output, session) {
  
  get_team2 <- function() {
    team1 <- c(input$team1_batters, input$team1_bowlers, input$team1_extra)
    all_players <- player_stats$Name
    setdiff(all_players, team1)
  }
  
  # Function to simulate innings scores
  simulate_innings <- function(team1_playing, team2_playing) {
    team1_scores <- player_stats %>%
      filter(Name %in% team1_playing) %>%
      mutate(Runs = round(sample(OA, n(), replace = TRUE)))
    
    team2_scores <- player_stats %>%
      filter(Name %in% team2_playing) %>%
      mutate(Runs = round(sample(OA, n(), replace = TRUE)))
    
    team1_total <- sum(team1_scores$Runs)
    team2_total <- sum(team2_scores$Runs)
    
    list(
      team1_scores_summary = team1_scores[, c("Name", "Runs")],
      team2_scores_summary = team2_scores[, c("Name", "Runs")],
      team1_total = team1_total,
      team2_total = team2_total
    )
  }
  
  # Simulate match results for two innings
  observeEvent(input$simulate_match, {
    team1 <- c(input$team1_batters, input$team1_bowlers, input$team1_extra)
    player_sitting_out <- sample(input$team1_extra, 1)
    team1_playing <- setdiff(team1, player_sitting_out)
    
    team2 <- get_team2()
    team2_extra <- extra_player_stats$Name[extra_player_stats$Name %in% team2]
    team2_sitting_out <- sample(team2_extra, 1)
    team2_playing <- setdiff(team2, team2_sitting_out)
    
    # Simulate Innings 1
    innings1 <- simulate_innings(team1_playing, team2_playing)
    # Simulate Innings 2
    innings2 <- simulate_innings(team1_playing, team2_playing)
    
    # Calculate grand totals
    team1_total_score <- innings1$team1_total + innings2$team1_total
    team2_total_score <- innings1$team2_total + innings2$team2_total
    
    # Determine winner based on aggregate scores
    winner <- ifelse(team1_total_score > team2_total_score, input$team1_name, "Not Your Team")
    
    # Display the winner along with grand totals
    output$winner_output <- renderText({
      paste("<h4><b>The winner is:</b>", winner, "</h4>",
            "<h5><b>", input$team1_name, "Total Runs:</b>", team1_total_score, "</h5>",
            "<h5><b>Not Your Team Total Runs:</b>", team2_total_score, "</h5>")
    })
    
    # Render match summary tables for both innings with totals row
    output$match_summary_innings1 <- renderTable({
      summary_table <- data.frame(
        Name_Team1 = c(innings1$team1_scores_summary$Name, "Total"),
        Runs_Team1 = c(innings1$team1_scores_summary$Runs, innings1$team1_total),
        Name_Team2 = c(innings1$team2_scores_summary$Name, "Total"),
        Runs_Team2 = c(innings1$team2_scores_summary$Runs, innings1$team2_total)
      )
      colnames(summary_table) <- c(input$team1_name, "Runs", "Not Your Team", "Runs")
      summary_table
    }, bordered = TRUE, spacing = "s", align = "l", rownames = FALSE)
    
    output$match_summary_innings2 <- renderTable({
      summary_table <- data.frame(
        Name_Team1 = c(innings2$team1_scores_summary$Name, "Total"),
        Runs_Team1 = c(innings2$team1_scores_summary$Runs, innings2$team1_total),
        Name_Team2 = c(innings2$team2_scores_summary$Name, "Total"),
        Runs_Team2 = c(innings2$team2_scores_summary$Runs, innings2$team2_total)
      )
      colnames(summary_table) <- c(input$team1_name, "Runs", "Not Your Team", "Runs")
      summary_table
    }, bordered = TRUE, spacing = "s", align = "l", rownames = FALSE)
  })
}

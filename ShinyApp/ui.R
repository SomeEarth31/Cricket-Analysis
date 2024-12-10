

# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(dplyr)

#Loading all the data

load("../Data/Ballers.RData")
load("../Data/Batters.RData")
load("../Data/Allrounders_ball.RData")
load("../Data/Allrounders_bat.RData")

# Code for UI
ui = fluidPage(
  theme = bs_theme(bootswatch = "cerulean"),
  # Main navigation
  navbarPage("Cricket Stats",
             
    # Player Data Tab
    tabPanel(
      "Player Data",
      sidebarLayout(
        sidebarPanel(
          # App Introduction Card
          div(class = "card",
              div(class = "card-body",
                  h5("App Overview"),
                  p("This app analyses the performance of the current top 10 test cricket players and checks whether the stats are inflated.")
              )
          ),
          br(),
          
          # Player Type and Name Selection
          selectInput(
            "type",
            "Select Player Type",
            choices = c("Batter", "Bowler", "All Rounder")
          ),
          selectInput(
            "players",
            "Select Player",
            choices = c()
          ),
         ),
        
        # Main panel with player information
        mainPanel(
          uiOutput("selected"),
          uiOutput("image_output"),
          
          # Display appropriate data tables based on player type
          uiOutput("data_table")
        )
      )
    ),
             
             # Second Panel is Simulation
             tabPanel("Match Simulation",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("team1_name", "Name Team 1:", "Your Team"),
                          selectizeInput("team1_batters", "Select 5 Batters for Team 1", 
                                         choices = BATTERS$OA$Name, multiple = TRUE, options = list(maxItems = 5)),
                          selectizeInput("team1_bowlers", "Select 5 Bowlers for Team 1", 
                                         choices = BALLERS$OA$Name, multiple = TRUE, options = list(maxItems = 5)),
                          selectizeInput("team1_extra", "Select the 11th and 12th Players for Team 1", 
                                         choices = c("Krati", "Soham", "Shreyasi", "Samarth"), multiple = TRUE, options = list(maxItems = 2)),
                          actionButton("simulate_match", "Simulate Match"),
                          width = 3
                        ),
                        mainPanel(
                          fluidRow(
                            column(6, 
                                   tags$div(h4("Match Summary - Innings 1"), tableOutput("match_summary_innings1"), style = "border: 1px solid #ccc; padding: 10px;")),
                            column(6, 
                                   tags$div(h4("Match Summary - Innings 2"), tableOutput("match_summary_innings2"), style = "border: 1px solid #ccc; padding: 10px;"))
                          ),
                          fluidRow(
                            column(12, 
                                   tags$div(h4("Match Winner"), htmlOutput("winner_output"), style = "border: 1px solid #ccc; padding: 10px;"))
                          ),
                          width = 9
                        )
                      )
             ),
             
             # Third Panel is Analysis with sub-tabs for each comparison plot
             tabPanel("Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "type1",
                            "Select Player Type",
                            choices = c("Batter", "Bowler", "All Rounder(Batting)", "All Rounder(Bowling)")
                          ),
                          width = 3
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Overall Comparison", plotOutput("plot1", height = "500px")),
                            tabPanel("Opposition-Wise Comparision", plotOutput("plot2", height = "500px")),
                            tabPanel("Host-Wise Comparision", plotOutput("plot3", height = "500px"))
                          ),
                          width = 9,
                          
                        )
                      )
             )
  )
)

# Server for First Pane
source("server1.R")
# Server for Second Pane
source("server2.R")
# Server for Third Pane
source("server3.R")

shinyApp(ui = ui, server = function(input, output, session) {
  server1(input, output, session)
  server2(input, output, session)
  server3(input, output, session)
})

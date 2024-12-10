#
# This is the server1 logic. 


library(shiny)
library(tidyverse)
load("../Data/Ballers.RData")
load("../Data/Batters.RData")
load("../Data/Allrounders_ball.RData")
load("../Data/Allrounders_bat.RData")

# Server Code
server1 <- function(input, output, session) {
  observeEvent(input$type, {
    if (input$type == "Batter") {
      updateSelectInput(session, "players", choices = BATTERS$Name)
    } else if (input$type == "Bowler") {
      updateSelectInput(session, "players", choices = BALLERS$Name)
    } else {
      updateSelectInput(session, "players", choices = ARS_bat$Name)
    }
  })
  
  datasetInput <- reactive({
    switch(input$type,
           "Batter" = BATTERS,
           "Bowler" = BALLERS,
           "All Rounder" = ARS_bat)
  })
  
  suffix <- reactive({
    switch(input$type,
           "Batter" = c("inns", "NO", "Avg"),
           "Bowler" = c("Runs", "Wkts", "Avg"),
           "All Rounder" = c("inns", "NO", "Avg"))
  })
  
  output$selected <- renderUI({
    h1(
      paste("Currently viewing:", input$players, "(", input$type, ")"),
      style = "font-size: 24px; font-weight: bold;"
    )
  })
  
  output$image_output <- renderUI({
    dat <- datasetInput()
    ind <- which(input$players == dat$Name)
    img(
      src = dat$Picture[ind],
      alt = "Selected Image",
      height = "400px",
      style = "border-radius: 10px; box-shadow: 5px 5px 10px rgba(0,0,0,0.2);"
    )
  })
  
  # Rename columns dynamically
  output$table11 <- renderTable({
    dat <- ARS_bat
    cols <- paste(input$players, c("inns", "NO", "Avg"))
    data <- dat$Against[c("Country", cols)]
    colnames(data)[2:4] <- c("Innings", "Not-outs", "Average")  # Renaming columns
    data
  })
  
  output$table12 <- renderTable({
    dat <- ARS_bat
    cols <- paste(input$players, c("inns", "NO", "Avg"))
    data <- dat$Host[c("Country", cols)]
    colnames(data)[2:4] <- c("Innings", "Not-outs", "Average")  # Renaming columns
    data
  })
  
  output$table21 <- renderTable({
    dat <- ARS_ball
    cols <- paste(input$players, c("Runs", "Wkts", "Avg"))
    data <- dat$Against[c("Country", cols)]
    colnames(data)[2:4] <- c("Runs Conceded", "Wickets Taken", "Average")  # Renaming columns
    data
  })
  
  output$table22 <- renderTable({
    dat <- ARS_ball
    cols <- paste(input$players, c("Runs", "Wkts", "Avg"))
    data <- dat$Host[c("Country", cols)]
    colnames(data)[2:4] <- c("Runs Conceded", "Wickets Taken", "Average")  # Renaming columns
    data
  })
  
  output$singleTable11 <- renderTable({
    dat <- datasetInput()
    cols <- paste(input$players, suffix())
    data <- dat$Against[c("Country", cols)]
    colnames(data)[2:4] <- c("Innings", "Not-outs", "Average")  # Renaming columns
    data
  })
  
  output$singleTable12 <- renderTable({
    dat <- datasetInput()
    cols <- paste(input$players, suffix())
    data <- dat$Host[c("Country", cols)]
    colnames(data)[2:4] <- c("Innings", "Not-outs", "Average")  # Renaming columns
    data
  })
  
  output$singleTable21 <- renderTable({
    dat <- datasetInput()
    cols <- paste(input$players, suffix())
    data <- dat$Against[c("Country", cols)]
    colnames(data)[2:4] <- c("Runs Conceded", "Wickets Taken", "Average")  # Renaming columns
    data
  })
  
  output$singleTable22 <- renderTable({
    dat <- datasetInput()
    cols <- paste(input$players, suffix())
    data <- dat$Host[c("Country", cols)]
    colnames(data)[2:4] <- c("Runs Conceded", "Wickets Taken", "Average")  # Renaming columns
    data
  })
  
  # Show both batting and bowling data for all-rounders, and respective data for batters and bowlers
  output$data_table <- renderUI({
    if (input$type == "All Rounder") {
      # Create tabs for all-rounders: Batting and Bowling Data
      tabsetPanel(
        tabPanel("Batting Data", 
                  fluidPage
                  (
                    fluidRow
                    (
                      column(6, h3("Player Data (Opposition Wise)"),tableOutput("table11")),
                      column(6, h3("Player Data (Host Wise)"),tableOutput("table12"))
                    )
                  )
                 ),
        tabPanel("Bowling Data",
                  fluidPage
                  (
                    fluidRow
                    (
                      column(6, h3("Player Data (Opposition Wise)"),tableOutput("table21")),
                      column(6, h3("Player Data (Host Wise)"),tableOutput("table22")),
                    )
                  )
                )
      )
    }
    else if(input$type == "Batter"){
      # Single table for batters
      fluidPage
      (
        fluidRow
        (
          column(6, h3("Player Data (Opposition Wise)"), tableOutput("singleTable11")),
          column(6, h3("Player Data (Host Wise)"), tableOutput("singleTable12"))
        )
      )
    }
    else{
      # Single table for bowlers
      fluidPage
      (
        fluidRow
        (
          column(6, h3("Player Data (Opposition Wise)"), tableOutput("singleTable21")),
          column(6, h3("Player Data (Host Wise)"), tableOutput("singleTable22"))
        )
      )
    }
  })
}

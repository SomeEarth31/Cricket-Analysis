
library(shiny)
library(tidyverse)

load("../Analysis/Allround_ball_host.RData")
load("../Analysis/Allround_ball_opp.RData")
load("../Analysis/Allround_bat_host.RData")
load("../Analysis/Allround_bat_opp.RData")
load("../Analysis/Ball_host.RData")
load("../Analysis/Ball_opp.RData")
load("../Analysis/Bat_host.RData")
load("../Analysis/Bat_opp.RData")
load("../Data/Ballers.RData")
load("../Data/Batters.RData")
load("../Data/Allrounders_ball.RData")
load("../Data/Allrounders_bat.RData")

server3 <- function(input, output, session) {
 
  { 
  ## Plot functions
  #p_plot1 <- function(dt1, dt2, OA, title, thing) {
  #  d2 <- data.frame(
  #    player = OA$Name, 
  #    `Strong_Average_Opp.` = dt1$`St. Avg`, 
  #    `Weak_Average_Opp.` = dt1$`W. Avg`, 
  #    `Overall_Average` = OA$OA,
  #    `Strong_Average_Host` = dt2$`St. Avg`, 
  #    `Weak_Average_Host` = dt2$`W. Avg`
  #  )
  #  
  #  batting_data_long <- d2 %>%
  #    pivot_longer(cols = c("Strong_Average_Opp.", "Weak_Average_Opp.", 
  #                          "Overall_Average", "Strong_Average_Host", "Weak_Average_Host"), 
  #                 names_to = "Category", 
  #                 values_to = "Average")
  #  
  #  plot = ggplot(batting_data_long, aes(x = player, y = Average, color = Category)) +
  #    geom_point(size = 4) +
  #    labs(title = paste("Comparison of", thing, "Averages between Strong and Weak Teams for", title),
  #         x = "Player", y = "Average") +
  #    theme_minimal() +
  #    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #  return(plot)
  #}
  }
  
  #Plot functions
  p_plot1 = function(dt1, dt2, OA, title, thing)
  {
    d2 = data.frame(player = OA$Name, `Strong_Average_Opp.` = dt1$`St. Avg`, 
                    `Weak_Average_Opp.` = dt1$`W. Avg`, `Overall_Average` = OA$OA,
                    `Strong_Average_Host` = dt2$`St. Avg`, `Weak_Average_Host` = dt2$`W. Avg`)
    
    # Convert data to long format for plotting
    batting_data_long <- d2 %>%
      pivot_longer(cols = c("Strong_Average_Opp.", "Weak_Average_Opp.", 
                            "Overall_Average", "Strong_Average_Host", 
                            "Weak_Average_Host"), names_to = "Category", values_to = "Average")
    
    # Plot
    plot = ggplot(batting_data_long, aes(x = player, y = Average, color = Category)) +
      geom_segment(aes(x = player, xend = player, y = min(Average) - 5, yend = max(Average) + 5), color = "grey80") +
      geom_point(size = 4) +
      labs(title = paste("Comparison of", thing, "Averages between Strong and Weak Teams for", title),
           x = "Player", y = "Batting Average") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(plot)
  }
  
  #Plot functions for Bowlers
  p_plot2 = function(dt1, dt2, OA, title, thing)
  {
    d2 = data.frame(player = OA$Name, `Strong_Average_Opp.` = dt1$`St. Avg`, 
                    `Weak_Average_Opp.` = dt1$`W. Avg`, `Overall_Average` = OA$OA)
    
    # Convert data to long format for plotting
    batting_data_long <- d2 %>%
      pivot_longer(cols = c("Strong_Average_Opp.", "Weak_Average_Opp.", 
                            "Overall_Average"), names_to = "Category", values_to = "Average")
    
    # Plot
    plot = ggplot(batting_data_long, aes(x = player, y = Average, color = Category)) +
      geom_segment(aes(x = player, xend = player, y = min(Average) - 5, yend = max(Average) + 5), color = "grey80") +
      geom_point(size = 4) +
      labs(title = paste("Comparison of", thing, "Averages between Strong and Weak Teams Opposition Wise for",title),
           x = "Player", y = "Batting Average") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(plot)
  }
  
  #Plot functions for Bowlers
  p_plot3 = function(dt1, dt2, OA, title, thing)
  {
    d2 = data.frame(player = OA$Name, `Overall_Average` = OA$OA,
                    `Strong_Average_Host` = dt2$`St. Avg`, `Weak_Average_Host` = dt2$`W. Avg`)
    
    # Convert data to long format for plotting
    batting_data_long <- d2 %>%
      pivot_longer(cols = c("Overall_Average", "Strong_Average_Host", 
                            "Weak_Average_Host"), names_to = "Category", values_to = "Average")
    
    # Plot
    plot = ggplot(batting_data_long, aes(x = player, y = Average, color = Category)) +
      geom_segment(aes(x = player, xend = player, y = min(Average) - 5, yend = max(Average) + 5), color = "grey80") +
      geom_point(size = 4) +
      labs(title = paste("Comparison of", thing, "Averages between Strong and Weak Teams Host Wise for",title),
           x = "Player", y = "Batting Average") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(plot)
  }
  
  # Render outputs in separate tabs
  output$plot1 <- renderPlot({
    switch(input$type1,
           "Batter" = p_plot1(Bat_opp, Bat_host, BATTERS$OA, "Batters", "Batting"),
           "Bowler" = p_plot1(Ball_opp, Ball_host, BALLERS$OA, "Bowlers", "Bowling"),
           "All Rounder(Batting)" = p_plot1(ARS_bat_opp, ARS_bat_host, ARS_bat$OA, "All Rounders", "Batting"),
           "All Rounder(Bowling)" = p_plot1(ARS_ball_op, ARS_ball_host, ARS_ball$OA, "All Rounders","Bowling"))
  })
  
  output$plot2 <- renderPlot({
    switch(input$type1,
           "Batter" = p_plot2(Bat_opp, Bat_host, BATTERS$OA, "Batters","Batting"),
           "Bowler" = p_plot2(Ball_opp, Ball_host, BALLERS$OA, "Bowlers", "Bowling"),
           "All Rounder(Batting)" = p_plot2(ARS_bat_opp, ARS_bat_host, ARS_bat$OA, "All Rounders","Batting"),
           "All Rounder(Bowling)" = p_plot2(ARS_ball_op, ARS_ball_host, ARS_ball$OA, "All Rounders", "Bowling"))
  })
  
  output$plot3 <- renderPlot({
    switch(input$type1,
           "Batter" = p_plot3(Bat_opp, Bat_host, BATTERS$OA, "Batters","Batting"),
           "Bowler" = p_plot3(Ball_opp, Ball_host, BALLERS$OA, "Bowlers", "Bowling"),
           "All Rounder(Batting)" = p_plot3(ARS_bat_opp, ARS_bat_host, ARS_bat$OA, "All Rounders","Batting"),
           "All Rounder(Bowling)" = p_plot3(ARS_ball_op, ARS_ball_host, ARS_ball$OA, "All Rounders", "Bowling"))
  })
}

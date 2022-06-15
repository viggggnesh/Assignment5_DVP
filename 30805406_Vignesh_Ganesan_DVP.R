library(tidyverse)
library(readxl)
library(readr)
library(rvest)
library(dplyr)
library(tools)
library(ggplot2)
library(lubridate)
library(Dict)
library(data.table)
library(ggraph)
library(ggrepel)
library(fmsb)
library(r2d3)
library(shiny)
library(shinyjs)
library(shinyBS)

ui <- fluidPage(
  useShinyjs(),
  
  tags$style("
    body{
    
    margin: 0;
    background: #ccba7c;
    background-attachment: fixed;
    background-position: center;
    background-repeat: no-repeat;
    background-size: cover;
  
    }
    
    #about {
    color: purple;
    }
    "),
  fluidRow(
    column(width = 12, offset = 6, 
           # Navigation button
           actionButton("navFwd", "Next"),
           actionButton("navBack", "Back"),
           ),
    
    fluidRow(
      column(width = 12,
             #Main Panel
             mainPanel(
               
               # Render HTML output
               
               # Introduction
               hidden(tags$div(id = "introduction",
                               HTML(paste("<p><h1>Welcome</h1></p>
                <p>This is a visualization narrative created to explain the performance of team Natus Vincere in the CS:GO Major championship held at Stockholm in early November 2021.</p>
                <p>Before the performance is analysed through the use of visuals, a brief walkthrough of certain elements must be done.</p>
                <p>Use the buttons to navigate through the content and click on highlighted links ", actionLink("tutorial_link", "such as this")," to display additional information. </p>"
                               ))
                               
               )
               ),    
               
               # About the game and certain rules
               hidden(tags$div(id = "about",
                               HTML(paste("<p><h1>CS:GO and Major</h1></p>
            <p>CS:GO (Counter Strike : Global Offensive) is a first person online multiplayer game that allows players to engage in a 5v5 battle for various tournaments. Among those is a tournament called the 'Major'.
                It is the most pretigious tournament with the highest prize pool.
            </p>
            <p>
            While the game can be traditionally classified as a victory through elimination due to the shooter nature of it, when playing at the highest professional level, it is way more complex than it appears
            <br>
            The game can be either won through - 
            <ul>
                <li>Elimination of enemy team</li>
                <li>Planting/defusing bomb depending on the side</li>
            </ul>
            The game can be won by winning 16/30 rounds in regulation (or by winning 4/6 when overtime rules are applied) over 7 selected maps that are selected by the game
            developers before the tournament.
            <br>
            Majors are events that consist of 16 teams of which 4 make the playoffs with 1 eventual champion. Currently there are 2 majors every", actionLink("year", "year.", ))))),
               
               # Motivation of the project
               hidden(tags$div(id = "data_viz",
                               HTML(paste("
            <p><h1>Data Visualization Project</h1></p>
            <p>
                The most recent even was won by the team Natus Vincere undefeated in every map/series they played. This project is a way to analyse the performance of the champions in comparison to other 
                teams at the event.
            </p>"))
               )
               ),
               
               # Player performance
               hidden(tags$div(id = "player_rating_dotplot",
                               HTML(paste(
                                 fluidRow(
                                   column(6, "graph here"),
                                   column(6, "text here")
                                 )
                               ))))
               
               ))
    )
  
  )
)

server <- function(input, output){
  
  # Initialize counter for navigation
  counter <- reactiveValues(cnt = 1)
  
  # Enable introduction panel
  toggle("introduction")
  
  # Reactive navigation
  observeEvent(input$navFwd, {
    
    counter$cnt <- counter$cnt + 1
    
    if(counter$cnt >= 5){counter$cnt <- 1}
  
    toggle("introduction", condition = {counter$cnt == 1})
    toggle("about", condition = {counter$cnt == 2})
    toggle("data_viz", condition = {counter$cnt == 3})
    toggle("player_rating_dotplot", condition = {counter$cnt == 4})
  })
  
  observeEvent(input$navBack, {
    
    counter$cnt <- counter$cnt - 1
    
    if(counter$cnt <= 0){counter$cnt <- 1}
    
    toggle("introduction", condition = {counter$cnt == 1})
    toggle("about", condition = {counter$cnt == 2})
    toggle("data_viz", condition = {counter$cnt == 3})
    toggle("player_rating_dotplot", condition = {counter$cnt == 4})
    
  })
  
  observeEvent(input$year, {
    showNotification(paste("XYZ 123"), closeButton = TRUE)
  })
  
  observeEvent(input$tutorial_link, {
    showNotification(paste("Pop up will appear with additional text. This can be closed."), closeButton = TRUE)
  })
}

# Trigger shinyApp
shinyApp(ui = ui, server = server)
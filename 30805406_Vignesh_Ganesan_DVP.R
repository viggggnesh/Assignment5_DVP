# Load library
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
library(DT)
library(ggiraph)
library(ggrepel)
library(fmsb)
library(r2d3)
library(plotly)
library(shiny)
library(shinyjs)
library(shinyBS)
library(rio)
library(devtools)

# loading csv file
df_ <- rio::import("rating_player.csv")

# Shiny UI
ui <- fluidPage(
  useShinyjs(),
  
  tags$style("
    body{
    
    margin: 15px;
    background-attachment: fixed;
    background-position: center;
    background-repeat: no-repeat;
    background-size: cover;
  
    }
    
    #introduction{
    align:center;
    }
    
    #about{
    
    }
    
    #data-viz{
    
    }
    
    #player_rating_dotplot{
    
    }
    
    "),

  fluidRow(align="center",
           fluidRow(
             # Navigation button
             actionButton("navFwd", "Next"),
             actionButton("navBack", "Back"),
           ),
           
           # Render HTML output
           
           # Introduction
           hidden(tags$div(id = "introduction",
                           HTML(paste("<h1>Welcome</h1>
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
                                      column(5, "width 5",
                                             fluidRow(plotOutput("rating_plot", click = "plot_click",
                                                      hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"), width = "100%"),
                                                      uiOutput("on_hover_info", style = "pointer-event:none")
                                                      ),
                                             fluidRow(
                                               div(style="margin:9.5px",
                                                 wellPanel(
                                                   checkboxGroupInput("team_placing", label = "Select event placement", inline = TRUE,
                                                                      choices = list("Winner"="1st",
                                                                                     "Runners-up"="2nd",
                                                                                     "3rd-4th"="3-4th",
                                                                                     "5th-8th"="5-8th",
                                                                                     "9th-12th"="9-12th",
                                                                                     "13th-16th"="13-16th",
                                                                                     "Avg. xPerformance"=9999),
                                                                      selected = c("1st","2nd",9999))
                                                 )
                                               )
                                             ),
                                             div(
                                               style="margin-top:15px",
                                               fluidRow(tableOutput("player_data"))
                                               )
                                             ),
                                      column(7, "width 7"))
                           ))))
           
           
          )
    
  
  
)

# Shiny server
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
  
  observeEvent(input$tutorial_link, {
    showNotification(paste("Pop up will appear with additional text. This can be closed."), closeButton = TRUE)
  })  
  
  observeEvent(input$year, {
    showNotification(paste("XYZ 123"), closeButton = TRUE)
  })

  output$rating_plot <- renderPlot({

    df_ <- rio::import("rating_player.csv")
    
    selectionPlacements <- input$team_placing
    
    df_ %>%
      filter(as.character(Year) %like% "2021") %>%
      filter(Placement %like% paste0(selectionPlacements,collapse = "|")) %>%
      
      # Plot options
      ggplot(mapping = aes(Year, Rating, color=Team, group=Name, tooltip=Name, data_id=Name)) +
      
      
      # Theme options
      theme_classic() + theme(legend.title = element_text(size=15)) + theme(legend.text = element_text(size=15)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + theme(axis.text.x = element_text(size=14)) +
      theme(axis.text.y = element_text(size=14)) + theme(axis.title = element_text(size=15)) +
      
      # Scale options
      scale_x_continuous("Days (increasing as tournament reaches conclusion)", labels = df_$Year, breaks = df_$Year) +
      ylim(0,2) + geom_point_interactive() + geom_line()

    })

  output$player_data <- renderTable({
    req(input$plot_click)

    df_ <- rio::import("rating_player.csv")

    nearPoints(df_,input$plot_click, xvar = "Year", yvar = "Rating", threshold = 5, maxpoints = 1, addDist = TRUE) %>%
      select(Name, Self, Teamplay, Support, Rating)
  }, width = 600)

  output$on_hover_info <- renderUI({
    
    df_ <- rio::import("rating_player.csv")
    
    on_hover <- input$plot_hover
    
    plot_hover(df_, on_hover)
  })
  
}

plot_hover <- function(df_, on_hover){
  point <- nearPoints(df_, on_hover, threshold = 5, maxpoints = 1, addDist = TRUE)
  
  if (nrow(point) == 0) return(NULL)
  
  left_pos <- on_hover$coords_css$x
  top_pos <- on_hover$coords_css$y
  
  # z-index is set so we are sure tooltip will be on top
  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                  "left:", left_pos + 2, "px; top:", top_pos + 2, "px;")
  
  # Tooltip
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Name: </b>", point$Name, "<br/>",
                  "<b> Team: </b>", point$Team, "<br/>"
    )))
  )  
  
}

# Trigger shinyApp
shinyApp(ui = ui, server = server)
######################
# Libraries required #
######################

# Install libraries using the code below


# Load library
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(Dict)
library(data.table)
library(htmlwidgets)
library(ggraph)
library(ggiraph)
library(DT)
library(fmsb)
library(rio)
library(shiny)
library(shinyjs)
library(shinyBS)

##################
# Shiny UI code  #
##################

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
    
    .box {
      float: left;
      height: 20px;
      width: 20px;
      margin-bottom: 15px;
      border: 1px solid black;
      clear: both;
    }
    
    .red {
      background-color: red;
    }
    
    .green {
      background-color: green;
    }
    
    .blue {
      background-color: blue;
    }
    
    
    "),

  fluidRow(align="center",
           fluidRow(
             # Navigation button
             actionButton("navFwd", "Next"),
             actionButton("navBack", "Back"),
             actionButton("navHelp", "Help"),
             hidden(actionButton("navReturn", "Return"))
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
           hidden(tags$div(id = "about", style="display: block; text-align: center;",
                           HTML(paste("<p><h1>CS:GO and Major</h1></p>
            <p>CS:GO (Counter Strike : Global Offensive) is a first person online multiplayer game that allows players to engage in a 5v5 battle for various tournaments. Among those is a tournament called the 'Major'.
                It is the most pretigious tournament with the highest prize pool.
            </p>
            <p>
            While the game can be traditionally classified as a victory through elimination due to the shooter nature of it, when playing at the highest professional level, it is way more complex than it appears
            <br>
            The game can be either won through -
            <ul style = 'display: inline-block; text-align: left;'>
                <li>Elimination of enemy team</li>
                <li>Planting/defusing bomb depending on the side</li>
            </ul>
            <br>
            The game can be won by winning 16/30 rounds in regulation (or by winning 4/6 when overtime rules are applied) over 7 selected maps that are selected by the game
            developers before the tournament.
            <br>
            Majors are events that consist of 16 teams of which 4 make the playoffs with 1 eventual champion. Currently there are 2 majors every", actionLink("year", "year.", ))))
            ),
           
           # Motivation of the project
           hidden(tags$div(id = "data_viz",
                           HTML(paste("
            <p><h1>Data Visualization Project</h1></p>
            <p>The most recent even was won by the team Natus Vincere undefeated in every map/series they played. This project is a way to analyse the performance of the champions in comparison to other
                teams at the event.
            </p>
            <p>Throughout this demo there are certain visual styles that are used which will be constant. This allows for easier tracking of data especially when dealig with different contexts everytime.</p>
            <p>
            <ul style = 'display: inline-block; text-align: left;'>
              <li>Terrorists (Attackers)</li>
              <li>Counter-Terrorists (Defenders)</li>
              <li>Average performance (any context)</li>
            </ul>
            </p>
            "))
           )
           ),
           
           # Player performance
           hidden(tags$div(id = "player_rating_dotplot",
                           
                             fluidRow(div(style="overflow-y:scroll;",
                                      column(6,
                                             # fluidRow(
                                             #   h1(style="text-align:center","Player performance rating for 2021 Major"),
                                             #   plotOutput("rating_plot", click = "plot_click",
                                             #              hover = hoverOpts("plot_hover", delay = 100,delayType = "debounce"), width = "100%"
                                             #              ),
                                             #   uiOutput("on_hover_info", style = "pointer-event:none")
                                             #          ),
                                             fluidRow(girafeOutput("rating_plot")),
                                             fluidRow(
                                               div(style="margin:9.5px; text-align:center;",
                                                 wellPanel(
                                                   checkboxGroupInput("team_placing", label = "Select event placement", inline = TRUE,
                                                                      choices = list("Winner"="1st",
                                                                                     "Runners-up"="2nd",
                                                                                     "3rd-4th"="3-4th",
                                                                                     "5th-8th"="5-8th",
                                                                                     "9th-12th"="9-12th",
                                                                                     "13th-16th"="13-16th"
                                                                                     ),
                                                                      selected = c("1st","2nd"))
                                                 )
                                               ),
                                               div(
                                                 style="margin-top:15px",
                                                 fluidRow(tableOutput("player_data"))
                                               )
                                             ),
                                             fluidRow(div(style="align-left;",
                                                div(style="margin:9px",plotOutput("player_radar_1", width = "100%")),
                                                div(style="margin:9px",plotOutput("player_radar_2", width = "100%"))
                                                      ))
                                             ),
                                      column(6,
                                             div(style="margin:15px;  text-align:justify",
                                               HTML(paste("<h1 style='text-align:center;'>Analysing the player ratings</h1>
                                                           <p>On the left a graph of the player performance is visible. In this instance, the rating used is a custom derived measure that uses other stats to rank players.
                                                           The criteria used is a combination of self performance, team play with kills and team support with utility. Click on a player to get a breakdown of their stats.</p>
                                                           <p>While it can be said that the players who win the tournament perform better, let's take a closer look at their individual performances.</p>
                                                          <p>Below the graph it is a table that shows the player breakdown versus the average performance of the tournament can be seen. The average rating is taken
                                                          from all matches that were not played by the finalists.</p>
                                                          <p>The stats contain a few terms :
                                                          <ol>
                                                          <li><strong>Self</strong> - A player's individual performance that counts the Kill+Assists by Death ratio and the multikill rounds they have.</li>
                                                          <li><strong>Teamplay</strong> - This is a measure of the actions that have a heavy impact on the round win of the team such as first kill, planting/defusing the bomb or even closing rounds with clutch kills.</li>
                                                          <li><strong>Support</strong> - Strong team support through other than kills that are taken by utility damage and flash assits.</li>
                                                          </ol>
                                                          </p>
                                                          <p>From this inital graph itself it is visible that the finalists ended up having almost similar levels of performance over the entire tournament, separated only by small margins.</p>
                                                          <p>Since there are a lot of players, individual comparison can only be done till a certain extent.</p>
                                                          ")) 
                                             )
                                            )
                                      ))
                           )),
           
           hidden(tags$div(id="players_and_teams",
                         HTML(paste(
                           "text"
                         ))))
           
           
          )
    
  
  
)

###############
# Server code #
###############

# Shiny server function
server <- function(input, output){
  
  # Initialize counter for page value as a reactive object
  counter <- reactiveValues(cnt = 1)
  
  # Enable introduction panel
  toggle("introduction")
  
  #######################
  # Reactive navigation #
  #######################
  
  # Navigate forward
  observeEvent(input$navFwd, {
    
    # Increment counter
    counter$cnt <- counter$cnt + 1
    
    if(counter$cnt >= 5){counter$cnt <- 1}
  
    toggle("introduction", condition = {counter$cnt == 1})
    toggle("about", condition = {counter$cnt == 2})
    toggle("data_viz", condition = {counter$cnt == 3})
    toggle("player_rating_dotplot", condition = {counter$cnt == 4})
  })
  
  # Navigate backwards
  observeEvent(input$navBack, {
    
    # Decrement counter
    counter$cnt <- counter$cnt - 1
    
    # Condition ensures you can never go a page back from the start page at any point
    if(counter$cnt <= 0){counter$cnt <- 1}
    
    # Toggle elements to appear like page changes based on condition
    toggle("introduction", condition = {counter$cnt == 1})
    toggle("about", condition = {counter$cnt == 2})
    toggle("data_viz", condition = {counter$cnt == 3})
    toggle("player_rating_dotplot", condition = {counter$cnt == 4})
    
  })
  
  # Navigate to help section
  observeEvent(input$navHelp, {
    
    # Toggle elements to appear like page changes based on condition
    hide("introduction")
    hide("about")
    show("data_viz")
    hide("player_rating_dotplot")
    hide("navFwd")
    hide("navBack")
    show("navReturn")
    
  })
  
  # Navigate to help section
  observeEvent(input$navReturn, {
    
    # Toggle elements to appear like page changes based on condition
    toggle("introduction", condition = {counter$cnt == 1})
    toggle("about", condition = {counter$cnt == 2})
    toggle("data_viz", condition = {counter$cnt == 3})
    toggle("player_rating_dotplot", condition = {counter$cnt == 4})
    toggle("navFwd")
    toggle("navBack")
    toggle("navReturn")
    
  })
  
  
  ##################
  # Pop up section #
  ##################
  
  # Helper popup
  observeEvent(input$tutorial_link, {
    showNotification(paste("Pop up will appear with additional text. This can be closed."), closeButton = TRUE)
  })
  
  # Information about major cycle
  observeEvent(input$year, {
    showNotification(paste("Every year CS:GO circuit holds 2 Major tournaments with 2020 to late 2021 being the only exception due to COVID-19."), closeButton = TRUE)
  })
  
  #########################
  # Output render section #
  #########################
  
  # # Plot for player rating 
  # output$rating_plot <- renderPlot({
  #   
  #   # Reading data from file
  #   df_ <- rio::import("rating_player.csv")
  #   df_$Year <- as.Date(df_$Year, format="%d-%m-%Y")
  #   
  #   # Get checkbox selection input
  #   selectionPlacements <- input$team_placing
  # 
  #   # Chain commands with the data frame  
  #   df_ %>%
  #     filter(as.character(Year) %like% "2021") %>%
  #     filter(Placement %like% paste0(selectionPlacements,collapse = "|")) %>%
  # 
  #     # Plot options
  #     ggplot(aes(Year, Rating, color=Team, group=Name)) +
  # 
  # 
  #     # Theme options
  #     theme_minimal() + theme(legend.title = element_text(size=15)) + theme(legend.text = element_text(size=15)) +
  #     theme(axis.text.x = element_text(angle = 25, vjust = 0.5)) + theme(axis.text.x = element_text(size=14)) +
  #     theme(axis.text.y = element_text(size=14)) + theme(axis.title = element_text(size=15)) +
  #     
  # 
  #     # Scale options
  #     scale_x_continuous("Days (increasing as tournament reaches conclusion)", labels = df_$Year, breaks = df_$Year) +
  #     ylim(0.5,2) + geom_point(aes(size=2.5)) +  scale_size(guide="none") + geom_line()
  # 
  # }, width = 900)
  
  output$rating_plot <- renderGirafe({
    
    df_ <- rio::import("rating_player.csv")
    df_$Year <- as.Date(df_$Year, format="%d-%m-%Y")
    
    # Get checkbox selection input
    selectionPlacements <- input$team_placing

    # Chain commands with the data frame
    girafe(ggobj = 
           df_ %>%
             filter(as.character(Year) %like% "2021") %>%
             filter(Placement %like% paste0(selectionPlacements,collapse = "|")) %>%
              
             # Plot options
             ggplot(aes(Year, Rating, color=Team, group=Name, tooltip=Team, data_id=Name)) +


             # Theme options
             theme_minimal() + theme(legend.title = element_text(size=15)) + theme(legend.text = element_text(size=15)) +
             theme(axis.text.x = element_text(angle = 25, vjust = 0.5)) + theme(axis.text.x = element_text(size=14)) +
             theme(axis.text.y = element_text(size=14)) + theme(axis.title = element_text(size=15)) +


             # Scale options
             scale_x_continuous("Days (increasing as tournament reaches conclusion)", labels = df_$Year, breaks = df_$Year) +
             scale_x_date(date_breaks = "1 day", date_labels =  "%b %d") +
             ylim(0.5,2) + geom_point_interactive(aes(size=2.5, onclick=onclick)) +  scale_size(guide="none") + geom_line(),
          
           # Girafe options 
          options = list(opts_selection(type = "single", only_shiny = FALSE)),width_svg = 9
          
    )
    
  })
  
  # On hover render information Panel
  output$on_hover_info <- renderUI({
    
    # Reading data from file
    df_ <- rio::import("rating_player.csv")
    df_$Year <- as.Date(df_$Year, format="%d-%m-%Y")
    
    # Prevents ghost input from occuring 
    req(input$plot_hover)
    
    on_hover <- input$plot_hover
    
    # Calling external function for generating tooltip
    plot_hover(df_, on_hover)
  })
  
  # Render player data into a table
  output$player_data <- renderTable({
    
    # Reading data from file
    df_ <- rio::import("rating_player.csv")
    df_$Year <- as.Date(df_$Year, format="%d-%m-%Y")
    
    req(input$plot_click)
    
    # Get average of all columns
    avg <- df_ %>%
      filter(as.character(Year) %like% "2021") %>%
      summarise_all(.funs = mean)
    
    avg$Placement <- as.character(avg$Placement) %>% replace_na('Avg')
    avg$Name <- as.character(avg$Name) %>% replace_na('Avg')
    avg$Team <- as.character(avg$Team) %>% replace_na('Avg')
    avg$Map <- as.character(avg$Map) %>% replace_na('Avg')
    avg$Map_Type <- as.character(avg$Map_Type) %>% replace_na('Avg')
    avg$Event <- as.character(avg$Event) %>% replace_na('Avg')
    
    # Subset data based on the point selected and display the result in a table.
    nearPoints(df_,input$plot_click, xvar = "Year", yvar = "Rating", threshold = 5, maxpoints = 1, addDist = TRUE) %>%
      select(Name,Team,Self,Teamplay,Support,Rating) %>%
      rbind(avg %>% select(Name,Team,Self,Teamplay,Support,Rating))
    
  }, width = 600)
  
  # Player performance breakdown radar chart based on individual values
  output$player_radar_1 <- renderPlot({
    
    req(input$plot_click)
    
    # Reading data from file
    df_ <- rio::import("rating_player.csv")
    df_$Year <- as.Date(df_$Year, format="%d-%m-%Y")
    
    # Set the colours for the radar chart
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9))
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4))
    
    # Get tournament average
    avg <- df_ %>%
      filter(as.character(Year) %like% "2021") %>%
      filter(!grepl(paste0(c("1st","2nd"),collapse = "|"), x = Placement)) %>%
      summarise_all(.funs = mean) %>% select(KaD, MK, Opk, Clt_won, Bmb_act, Trading, Eff_flash, Utility)
    
    # Subset the data frame based on the plot get the mean rating for a player
    y <- df_ %>%
      filter(as.character(Year) %like% "2021",
             Name %like% paste(nearPoints(df_,input$plot_click, xvar = "Year", yvar = "Rating", threshold = 5, maxpoints = 1, addDist = TRUE) %>% select(Name))) %>%
      select(KaD, MK, Opk, Clt_won, Bmb_act, Trading, Eff_flash, Utility) %>%
      summarise_all(mean)
    
    # Get min and max of every category
    mins <- bind_rows(apply(df_ %>%
                              filter(as.character(Year) %like% "2021") %>%
                              select(KaD, MK, Opk, Clt_won, Bmb_act, Trading, Eff_flash, Utility),2,min))
    
    maxs <- bind_rows(apply(df_ %>%
                              filter(as.character(Year) %like% "2021") %>%
                              select(KaD, MK, Opk, Clt_won, Bmb_act, Trading, Eff_flash, Utility),2,max))
    
    # Bind output rows with min and max at the top  
    y <- rbind(mins,maxs,avg,y)
    
    # Radar chart command
    radarchart(y, axistype = 0,
              pcol=colors_border , pfcol=colors_in , plwd=1 , plty = 1,
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8, vlcex=0.8, 
              title = "Performance breakdown",
              vlabels = c("Kills/Assists per Death", "1+ Kills","Entry kills", "1 vs X clutches", "Actions on bomb", "Trading", "Flash assists", "Utility support"), maxmin = TRUE)
    
    # Legend options  
    legend("topleft", legend = c("Tournament Avg.","Player"), bty = "n", pch=20 , col=colors_in , text.col = "grey25", cex=1.2, pt.cex=3)
      
  
  },width = 800)
  
  # Player performance breakdown radar chart based on semi aggregated
  output$player_radar_2 <- renderPlot({
    
    req(input$plot_click)
    
    # Reading data from file
    df_ <- rio::import("rating_player.csv")
    df_$Year <- as.Date(df_$Year, format="%d-%m-%Y")

    # Set the colours for the radar chart
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9))
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4))

    # Get tournament average
    avg <- df_ %>%
      filter(as.character(Year) %like% "2021") %>%
      filter(!grepl(paste0(c("1st","2nd"),collapse = "|"), x = Placement)) %>%
      summarise_all(.funs = mean) %>% select(Self, Teamplay, Support, Rating)

    # Subset the data frame based on the plot get the mean rating for a player
    y <- df_ %>%
      filter(as.character(Year) %like% "2021",
             Name %like% paste(nearPoints(df_,input$plot_click, xvar = "Year", yvar = "Rating", threshold = 5, maxpoints = 1, addDist = TRUE) %>% select(Name))) %>%
      select(Self, Teamplay, Support, Rating) %>%
      summarise_all(mean)
  
    # Get min and max of every category
    mins <- bind_rows(apply(df_ %>%
                              filter(as.character(Year) %like% "2021") %>%
                              select(Self, Teamplay, Support, Rating),2,min))

    maxs <- bind_rows(apply(df_ %>%
                              filter(as.character(Year) %like% "2021") %>%
                              select(Self, Teamplay, Support, Rating),2,max))
  
    # Bind output rows with min and max at the top  
    y <- rbind(mins,maxs,avg,y)

    # Radar chart command
    radarchart(y, axistype = 0,
               pcol=colors_border , pfcol=colors_in , plwd=1 , plty = 1,
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8, vlcex=0.8,
               title = "Performance breakdown",
               vlabels = c("Self","Teamplay","Support"))

    # Legend options 
    legend("topright", legend = c("Tournament Avg.","Player"), bty = "n", pch=20 , col=colors_in , text.col = "grey25", cex=1.2, pt.cex=3)

  },width = 800)

}

########################
# External function(s) #
########################

# On-hover function
plot_hover <- function(df_, on_hover){
  point <- nearPoints(df_, on_hover, threshold = 5, maxpoints = 1, addDist = TRUE)
  
  # Return NULL on point click 0
  if (nrow(point) == 0) return(NULL)
  
  # Get point coordinates when hovering over a plot
  left_pos <- on_hover$coords_css$x
  top_pos <- on_hover$coords_css$y
  
  # Tooltip style options
  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                  "left:", left_pos + 2, "px; top:", top_pos + 2, "px;")
  
  # Tooltip output contents
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Name: </b>", point$Name, "<br/>",
                  "<b> Team: </b>", point$Team, "<br/>",
                  "<b> Position: </b>", point$Placement, "<br/>"
    )))
  )
}


# Trigger shinyApp by passing UI and server objects
shinyApp(ui = ui, server = server)
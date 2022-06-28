library(plotly)
library(tidyverse)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  
  HTML(paste(plotlyOutput("sample_plot")))
  
)

server <- function(input, output, session) {
  
  output$sample_plot <- renderPlotly({
    
    df_ <- read_csv("rating_player.csv")
    df_$Year <- as.Date(df_$Year, format="%d-%m-%Y")
    
    # Get checkbox selection input
    selectionPlacements <- input$team_placing
    
    # Chain commands with the data frame
    ggplotly (df_ %>%
                filter(as.character(Year) %like% "2021") %>%
                filter(Placement %like% paste0(selectionPlacements,collapse = "|")) %>%
                
                # Plot options
                ggplot(aes(Year, Rating, color=Team, group=Name)) + geom_point(aes(size=2.5))
    )
    
  })
  
}

shinyApp(ui, server)
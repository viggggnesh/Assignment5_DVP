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

ui <- fluidPage(
  useShinyjs(),
  # Title
  titlePanel("Welcome"),
  
  #Sidebar Panel
  
  #Main Panel
  mainPanel(
    
    # Button
    actionButton("navFwd", "fwd"),
    actionButton("navBack", "back"),
    hidden(textOutput("Introduction to th")),
    hidden(textOutput("text2"))
    
    # # Output: Tabset w/ plot, summary, and table ----
    # tabsetPanel(type = "tabs",
    #             tabPanel("Plot", titlePanel("Tab1")),
    #             tabPanel("Summary", titlePanel("Tab2")),
    #             tabPanel("Table", titlePanel("Tab3"))
    # )
  )
)

server <- function(input, output){
  
  counter <- reactiveValues(cnt = 1)
  
  toggle("text1")
  
  output$text1 <- renderText("text 1 visible now")
  
  output$text2 <- renderText("text 2 visible now")
  
  observeEvent(input$navFwd, {
    
    counter$cnt <- counter$cnt + 1
    
    toggle("text1", condition = {counter$cnt == 1})
    
    toggle("text2", condition = {counter$cnt >= 2})
    
  })
  
  observeEvent(input$navBack, {
    
    counter$cnt <- counter$cnt - 1
    
    toggle("text1", condition = {counter$cnt <= 1})
    
    toggle("text2", condition = {counter$cnt >= 2})
    
  })
}

# Trigger shinyApp
shinyApp(ui = ui, server = server)
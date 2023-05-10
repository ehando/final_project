library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(shiny)
library(leaflet)
rm(list = ls())

#setwd("C:/Users/Eirik/OneDrive/College/Senior/Data 332/final_project")


# read csv file
df <- read.csv("data/hotels_motels.csv") %>%
  separate(the_geom, into = c(NA, "longitude", "latitude"), sep = " ")

# remove parentheses from latitude and longitude columns
df$latitude <- gsub("\\(|\\)", "", df$latitude)
df$longitude <- gsub("\\(|\\)", "", df$longitude)

# convert latitude and longitude columns to numeric values
df$latitude <- as.numeric(df$latitude)
df$longitude <- as.numeric(df$longitude)  


# Define the UI
ui <- fluidPage(
  titlePanel("Stays in New Orleans"),
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction",
                 textOutput("intro_text")),
        tabPanel("Map of Hotels, Motels, and Rentals",
                fluidRow(
                  column(12, leafletOutput("map"))
                  )),
        tabPanel("Count by Business Type",
                 fluidRow(
                   column(12, plotOutput("busn_type_plot"))
                 )),
        tabPanel("Business Start Date",
                 fluidRow(
                   column(12, plotOutput("start_date"))
                 ))
        
      )))

# Define the server
server <- function(input, output) {
  
  #Intro paragraph explaining project
  output$intro_text <-renderText({
    "Explanation of our project"
  })
  
  # Count by busn type
  output$busn_type_plot <- renderPlot({
    # pivot showing count of each busn type
    busn_type <- df %>%
      group_by(BusinessType) %>%
      summarise(count = n())
    
    ggplot(busn_type, aes(x = BusinessType, y = count)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = "Business Type", y = "Count", title = "Business Type Counts")
  })
  
  # Map of Places
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = df, lat = ~latitude, lng = ~longitude)
  })
  
  # Business Start Date
  output$start_date <- renderPlot({
    start_date <- df %>%
      mutate(Year = lubridate::year(BusinessStartDate)) %>%
      group_by(Year, BusinessType) %>%
      summarise(count = n()) %>%
      arrange(Year, BusinessType) %>%
      group_by(BusinessType) %>%
      mutate(cumulative_count = cumsum(count))
    
    ggplot(start_date, aes(x = Year, y = cumulative_count,
                           color = BusinessType,group = BusinessType)) +
      geom_line(linewidth = 1.1) +
      scale_color_viridis(discrete = TRUE, option = "H") +
      labs(x = "Business Start Date", y = "Count") +
      ggtitle("Trend of New Business Start Dates")
  })
}

shinyApp(ui, server)


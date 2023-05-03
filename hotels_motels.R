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
  separate(the_geom, into = c("type", "longitude", "latitude"), sep = " ")

# remove parentheses from latitude and longitude columns
df$latitude <- gsub("\\(|\\)", "", df$latitude)
df$longitude <- gsub("\\(|\\)", "", df$longitude)

# convert latitude and longitude columns to numeric values
df$latitude <- as.numeric(df$latitude)
df$longitude <- as.numeric(df$longitude)  

# Define the UI
ui <- fluidPage(
  leafletOutput("map"),
  plotOutput("barplot")
)

# Define the server
server <- function(input, output) {
  
  # Render the bar chart
  output$barplot <- renderPlot({
    # pivot showing count of each busn type
    busn_type <- df %>%
      group_by(BusinessType) %>%
      summarise(count = n())
    
    ggplot(busn_type, aes(x = BusinessType, y = count)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = "Business Type", y = "Count", title = "Business Type Counts")
  })
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = df, lat = ~latitude, lng = ~longitude)
  })
}

shinyApp(ui, server)
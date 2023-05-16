library(tidyverse)
library(tidytext)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(shiny)
library(leaflet)
library(caret)
library(class)
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
                 )),
        tabPanel("Major Players in the Business",
                 fluidRow(
                   column(12, plotOutput("major_players"))
                 )),
        tabPanel("LLM Prediction Model",
                 fluidRow(
                   column(12, textOutput("llm_model"))
                 )),
        tabPanel("Our Prediction Model",
                 fluidRow(
                  column(6,textInput("latitude_input", "Latitude:",
                                     placeholder = "Enter latitude...")),
                  column(6, textInput("longitude_input", "Longitude:",
                                     placeholder = "Enter longitude..."))),
                 fluidRow(
                  column(12, actionButton("predict_button", "Predict"))),
                 fluidRow(
                  column(12, verbatimTextOutput("predicted_output"))
                 ))
      )))

# Define the server
# Define the server
server <- function(input, output) {
  
  #Intro paragraph explaining project
  #Intro paragraph explaining project
  output$intro_text <-renderText({
    "
    
    
   
   We found the data on data.gov. It contains information on hotels and motels in the LA neighborhood of New Orleans. 
   We created a shiny app to begin the analysis. 
   We divided latitude and longitude, which made it easier for us to locate various places on the map. 
   To determine the number of hotels and motels operating there, we then counted different company categories and created a bar graph. 
   We used a company start date and kept track of how many companies launched over the course of a decade in order to conduct further analyses. 
   To determine the terms that were utilized in the business type, we used emotional analysis. 
   The most repeated words were then counted once we had separated the words. 
   When we examine the top 5 companies in the Major Player in the Business graph, 
   we can see that Sonder is the largest chain when compared to the others. Additionally, 
   we developed a prediction model based on latitude and longitude that requests the user's input in order 
   to inform them of the location of each organization.
   


"
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
                           color = BusinessType, group = BusinessType)) +
      geom_line(linewidth = 1.1) +
      scale_color_viridis(discrete = TRUE, option = "H") +
      labs(x = "Business Start Date", y = "Count") +
      ggtitle("Trend of New Business Start Dates")
  })
  
  # The Major Players in NOLA
  output$major_players <- renderPlot({
    word_counts <- df %>%
      unnest_tokens(word, BusinessName) %>%
      count(word) %>%
      arrange(desc(n))
    
    string_values <- c('sonder', 'marriott', 'hilton', 'holiday', 'hyatt')
    majors <- subset(word_counts, word %in% string_values)

    ggplot(majors, aes(x = word, y = n)) +
      geom_bar(stat = "identity")  
  })
  
  output$llm_model <- renderText({
    data <- df
    data$year <- substr(data$BusinessStartDate, 1, 4)
    
    # Split data into training and testing sets
    set.seed(123)
    trainIndex <- createDataPartition(data$BusinessType, p = 0.8, list = FALSE)
    training <- data[trainIndex, ]
    testing <- data[-trainIndex, ]
    
    # Split the response variables
    training$latitude <- as.numeric(training$latitude)
    training$longitude <- as.numeric(training$longitude)
    
    # Convert 'year' to character in both training and testing datasets
    training$year <- as.numeric(training$year)
    testing$year <- as.numeric(testing$year)
    
    # Train the model separately for latitude and longitude
    model_latitude <- train(latitude ~ year + BusinessType,
                            method = "rpart",
                            data = training)
    
    model_longitude <- train(longitude ~ year + BusinessType,
                             method = "rpart",
                             data = training)
    
    # Predict the future locations for each BusinessType
    predictions_latitude <- predict(model_latitude, newdata = testing)
    predictions_longitude <- predict(model_longitude, newdata = testing)
    
    # Specify the latitude and longitude
    lat <- predictions_latitude[1]
    lng <- predictions_longitude[1]
    
    # Predicted location of next business
    print(lat)
    print(lng)
    '
    # Location is 611 Bourbon St, New Orleans, LA 70130, United States
    
    
    # APA Citation from Bing AI
    "Built In. (n.d.). Regression Trees: How to Get Started. Built In.
    https://builtin.com/data-science/regression-tree"
    
    "Carnegie Mellon University. (n.d.). Lecture 10: Regression Trees.
    Carnegie Mellon University. https://www.stat.cmu.edu/~cshalizi/350-2006/lecture-10.pdf"
    
    # Explanation
    "Regression trees are a variant of decision trees that aim to
    predict outcomes we consider real numbers — such as the optimal prescription
    dosage, the cost of gas next year or the number of expected Covid cases this
    winter. Regression trees divide the data into subsets, that is, branches,
    nodes, and leaves. Like decision trees, regression trees select splits that
    decrease the dispersion of target attribute values. Thus, the target attribute
    values can be predicted from their mean values in the leaves."
    '
  })
  
  observeEvent(input$predict_button, {
  
    dataset <- df[, c("latitude", "longitude", "BusinessType")]
    
    # Split the dataset into training and testing sets
    set.seed(123) # Set seed for reproducibility
    train1_indices <- sample(nrow(dataset), nrow(dataset)*0.7) # 70% for training
    train1_data <- dataset[train1_indices, ]
    test1_data <- dataset[-train1_indices, ]
    
    # Set the number of neighbors for the KNN model
    k <- 3
    
    # Train the KNN model
    knn_model <- knn(train1_data[, c("latitude", "longitude")],
                     test1_data[, c("latitude", "longitude")],
                     train1_data$BusinessType, k)
    
    # Evaluate the model
    accuracy <- sum(knn_model == test1_data$BusinessType) / 
      length(test1_data$BusinessType)
    
    cat("Accuracy:", accuracy, "\n")
    
    # Make predictions on new data
    # Get user input
    latitude <- as.numeric(input$latitude_input)
    longitude <- as.numeric(input$longitude_input)
    
    # Validate input
    if (is.na(latitude) || is.na(longitude)) {
      output$predicted_output <- renderPrint("Invalid input. Please enter numeric values.")
    } else {
      # Make predictions on new data
      new_data <- data.frame(latitude = latitude,
                             longitude = longitude)
      predicted_types <- knn(train1_data[, c("latitude", "longitude")], new_data,
                             train1_data$BusinessType, k)
      
      # Display predicted business types
      output$predicted_output <- renderPrint({
        cat("Predicted Business Type:", predicted_types, "\n")
        predicted_types
      })
    }})

  
}

shinyApp(ui, server)


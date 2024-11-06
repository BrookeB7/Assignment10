library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)
library(leaflet.extras)
library(ggiraph)
library(shinyWidgets)

# Load and preprocess the data
accident <- read_csv("accident.csv") %>%
  filter(HOUR <= 24, LATITUDE < 77)

# Define UI
ui <- fluidPage(
  titlePanel("Accident Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("state", "Select State:", choices = c("All", unique(accident$STATENAME)), 
                  selected = c("All", unique(accident$STATENAME)), # Default select all
                  options = list(`actions-box` = TRUE), multiple = TRUE),
      pickerInput("weather", "Select Weather Condition:", choices = c("All", unique(accident$WEATHERNAME)), 
                  selected = c("All", unique(accident$WEATHERNAME)), # Default select all
                  options = list(`actions-box` = TRUE), multiple = TRUE),
      pickerInput("light", "Select Light Condition:", choices = c("All", unique(accident$LGT_CONDNAME)), 
                  selected = c("All", unique(accident$LGT_CONDNAME)), # Default select all
                  options = list(`actions-box` = TRUE), multiple = TRUE),
      pickerInput("route", "Select Route Name:", choices = c("All", unique(accident$ROUTENAME)), 
                  selected = c("All", unique(accident$ROUTENAME)), # Default select all
                  options = list(`actions-box` = TRUE), multiple = TRUE),
      sliderInput("fatals", "Select Number of Fatals:", 
                  min = min(accident$FATALS, na.rm = TRUE), 
                  max = max(accident$FATALS, na.rm = TRUE), 
                  value = c(min(accident$FATALS, na.rm = TRUE), max(accident$FATALS, na.rm = TRUE)))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Number of Accidents by Hour", plotOutput("accidentsPlot")),
        tabPanel("Day of Week Histogram", plotOutput("dayWeekPlot")),
        tabPanel("Accident Map", leafletOutput("accidentMap"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  filtered_data <- reactive({
    data <- accident
    if (!("All" %in% input$state)) data <- data %>% filter(STATENAME %in% input$state)
    if (!("All" %in% input$weather)) data <- data %>% filter(WEATHERNAME %in% input$weather)
    if (!("All" %in% input$light)) data <- data %>% filter(LGT_CONDNAME %in% input$light)
    if (!("All" %in% input$route)) data <- data %>% filter(ROUTENAME %in% input$route)
    data %>% filter(FATALS >= input$fatals[1] & FATALS <= input$fatals[2])
  })
  
  # Number of Accidents by Hour Plot
  output$accidentsPlot <- renderPlot({
    hourly_counts <- filtered_data() %>%
      group_by(HOUR) %>%
      summarise(Accidents = n())
    
    ggplot(hourly_counts, aes(x = HOUR, y = Accidents)) +
      geom_line(color = "red", size = 1) +
      geom_point(color = "red", size = 3) +
      geom_smooth(method = "loess", se = TRUE, color = NA, fill = "lightblue") +
      labs(title = "Accidents by Hour of Day", x = "Hour of Day", y = "Number of Accidents") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 12),
            legend.position = "none")
  })
  
  # Day of Week Histogram with Percentages
  output$dayWeekPlot <- renderPlot({
    day_counts <- filtered_data() %>%
      mutate(DAY_WEEKNAME = factor(DAY_WEEKNAME, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      group_by(DAY_WEEKNAME) %>%
      summarise(Accidents = n()) %>%
      mutate(Percentage = Accidents / sum(Accidents) * 100)
    
    ggplot(day_counts, aes(x = DAY_WEEKNAME, y = Accidents, fill = DAY_WEEKNAME)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 4) +
      scale_fill_brewer(palette = "Set3") +
      labs(
        title = "Accident Distribution by Day of the Week",
        x = "Day of the Week",
        y = "Total Number of Accidents",
        fill = "Day of the Week"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.6, hjust = 0.5),
        legend.position = "none"
      )
  })
  
  # Leaflet Map with Marker Clustering
  output$accidentMap <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(~LONGITUD, ~LATITUDE,
                       clusterOptions = markerClusterOptions(),
                       popup = ~paste("State:", STATENAME, "<br>",
                                      "Weather:", WEATHERNAME, "<br>",
                                      "Light Condition:", LGT_CONDNAME, "<br>",
                                      "Hour:", HOUR))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

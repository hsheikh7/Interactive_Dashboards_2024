# Load required libraries
library(shiny)
library(plotly)
library(DT)

# Load the dataset
data <- read.csv("data/Spain_UrbanMobility_Covid19.csv")
data$date <- as.Date(data$date)
data$region <- as.factor(data$region)

colnames(data) <- c("country", "region", "sub_region", "date", "retail_recreation", "grocery_pharmacy", "parks", "transit_stations", "workplaces", "residential")

# Set default values
default_country <- "Spain"
default_region <- "Total"

ui <- fluidPage(
  
  titlePanel("COVID-19 Urban Mobility Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(data$country), selected = default_country),
      selectInput("region", 
                  "Select Region:", 
                  choices = unique(data$region), 
                  selected = default_region,
                  multiple = TRUE),
      selectInput("category", "Select Category:", choices = c("retail_recreation", "grocery_pharmacy", "parks", "transit_stations", "workplaces", "residential")),
      dateRangeInput("dates", "Select Date Range:", start = min(data$date), end = max(data$date)),
      downloadButton("downloadData", "Download Data")
    ),
    
    mainPanel(
      plotlyOutput("dataPlot"),
      dataTableOutput("dataTable")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Update regions based on selected country
  observeEvent(input$country, {
    updateSelectInput(session, "region", choices = unique(data[data$country == input$country, "region"]), selected = default_region)
  })
  
  filteredData <- reactive({
    subset(data, country == input$country & region %in% input$region & 
             date >= input$dates[1] & date <= input$dates[2])
  })
  
  output$dataPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes_string(x = "date", y = input$category, color = "region")) +
      geom_point(alpha = 0.5) + theme(legend.position = "none") +
      ylab("% change from baseline")
    
    ggplotly(p)
  })
  
  output$dataTable <- DT::renderDataTable({
    filteredData()
  })
  
  # Download Data 
  output$downloadData <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filteredData()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
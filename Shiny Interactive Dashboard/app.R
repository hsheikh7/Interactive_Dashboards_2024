# Load required libraries
library(shiny)
library(plotly)
library(DT)

# Load the dataset
data <- read.csv("data/mobility_report_countries.csv")

# Update column names to match the variable names used in the code
colnames(data) <- c("country", "region", "date", "retail_recreation", "grocery_pharmacy", "parks", "transit_stations", "workplaces", "residential")

# Define UI for application
ui <- fluidPage(
  
  titlePanel("Mobility Data Visualization Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(data$country)),
      selectInput("region", "Select Region:", choices = unique(data$region)),
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
server <- function(input, output) {
  
  filteredData <- reactive({
    subset(data, country == input$country & region == input$region & date >= input$dates[1] & date <= input$dates[2])
  })
  
  output$dataPlot <- renderPlotly({
    category_selected <- input$category
    plot_ly(filteredData(), x = ~date, y = ~get(category_selected), type = 'scatter', mode = 'lines') %>%
      layout(title = paste(category_selected, "trend in", input$country, "-", input$region), xaxis = list(title = "Date"), yaxis = list(title = category_selected))
  })
  
  output$dataTable <- renderDataTable({
    filteredData()
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)
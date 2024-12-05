# Load necessary libraries
library(shiny)
library(DT)

# Read the CSV file
data <- read_csv("Championships.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Dual Meet Wrestling Data Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("search", "Search:", placeholder = "Enter keyword to search"),
      selectInput("filter_col", "Filter by column:",
                  choices = colnames(data), selected = "Year"),
      textInput("filter_value", "Filter value:", placeholder = "Enter value to filter")
    ),
    mainPanel(
      DTOutput("data_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    search_query <- input$search
    filter_col <- input$filter_col
    filter_value <- input$filter_value
    
    # Apply filtering
    filtered <- data
    if (!is.null(filter_value) && filter_value != "") {
      filtered <- filtered[filtered[[filter_col]] == filter_value, ]
    }
    if (!is.null(search_query) && search_query != "") {
      filtered <- filtered[apply(filtered, 1, function(row) {
        any(grepl(search_query, row, ignore.case = TRUE))
      }), ]
    }
    filtered
  })
  
  # Render data table
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, searchHighlight = TRUE))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

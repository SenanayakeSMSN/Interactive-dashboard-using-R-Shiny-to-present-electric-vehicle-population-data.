library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(ggplot2)
library(plotly)  # for interactive plots

# Load the dataset
ev_data <- read.csv('Electric_Vehicle_Population_Data.csv')


ui <- dashboardPage(
  dashboardHeader(title = "Electric Vehicles Population"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    uiOutput("page")
  )
)

server <- function(input, output, session) {
  
  output$page <- renderUI({
    if (input$tabs == "Home") {
      fluidRow(
        box(
          title = "",
          div(
            h3("Welcome to the Electric Vehicles Population Dashboard", style = "color: #1f77b4; font-family: Arial, sans-serif; font-size: 24px; font-weight: bold;"),
            p("This dashboard provides information about electric vehicle population data. It includes various statistics, trends, and visualizations related to the adoption and usage of electric vehicles across different regions and time periods.", style = "color: #333; font-family: Arial, sans-serif; font-size: 16px;"),
            p("You can explore the data through interactive charts, tables, maps, and more to gain insights into the growth of electric vehicle populations and their impact on energy consumption and environmental sustainability.", style = "color: #333; font-family: Arial, sans-serif; font-size: 16px;")
          ),
          width = 12,  # Adjust the width of the box
          style = "overflow-x: auto; overflow-y: auto;"
        )
      )
    } else if (input$tabs == "dashboard") {
      fluidRow(
        box(
          width = 12,
          title = "Electric Vehicle Population Data",
          DTOutput("ev_table"),  # Use DTOutput for the DataTable
          style = "overflow-x: auto; overflow-y: auto;"
        ),
        box(plotOutput("pie_chart")),  # Add plot output for pie chart
        box(plotOutput("histogram")), # Add plot output for histogram
        
        box(
          width = 12,
          selectInput("make_filter", "Select Vehicle Brand:", choices = c("Select All", unique(ev_data$Make)), multiple = TRUE, selected = "Select All")
        ),
        box(
          width = 12,  
          plotOutput("bar_chart")
        ),
        
        box(
          width = 12,  
          plotlyOutput("scatter_plot")
        ),
        
        
        box(
          width = 12,  
          plotOutput("box_plot")
        )
      )
    }
  })


  
  # Render histogram
  output$histogram <- renderPlot({
    hist(ev_data$Electric_Range, main = "Histogram of Electric Range", xlab = "Values", col = "#1f77b4", border = "white", las = 1)
  })
  
  # Render pie chart
  output$pie_chart <- renderPlot({
    data <- table(sample(c("Battery Electric Vehicle (BEV)", "Plug-in Hybrid Electric Vehicle (PHEV)"), size = 100, replace = TRUE))
    pie(data, main = "Pie Chart for Electric Vehicle Type", col = c("#1f77b4", "#aec7e8"), labels = paste0(names(data), ": ", round(100*data/sum(data), 1), "%"))
  })
  
  # Render data table
  output$ev_table <- renderDT({
    datatable(ev_data, filter = 'top', options = list(
      search = list(search = ""),  # Set default search value to empty
      columnDefs = list(list(targets = 2, searchable = TRUE)),  # Enable searching for the second column (assuming it's the column where the vehicle numbers are located)
      pageLength = 3  # Show only 3 rows per page initially
    ))
  })
  
  # Render bar chart
  output$bar_chart <- renderPlot({
    if ("Select All" %in% input$make_filter) {
      filtered_data <- ev_data
    } else {
      filtered_data <- ev_data[ev_data$Make %in% input$make_filter, ]
    }
    ggplot(filtered_data, aes(x = Make)) + 
      geom_bar(fill = "#1f77b4") +  
      labs(title = "Distribution of Electric Vehicle Makes (select vehicle brands from above)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # Rotate x-axis labels vertically
  })
  
  # Render scatter plot
  output$scatter_plot <- renderPlotly({
    if ("Select All" %in% input$make_filter) {
      filtered_data <- ev_data
    } else {
      filtered_data <- ev_data[ev_data$Make %in% input$make_filter, ]
      
    }
    
    plot_ly(filtered_data, x = ~Base_MSRP, y = ~Electric_Range, type = 'scatter', mode = 'markers') %>%
      layout(title = "Scatter Plot of Base MSRP vs. Electric Range", xaxis = list(title = "Base MSRP"), yaxis = list(title = "Electric Range"))
  })
  
  # Render box plot
  output$box_plot <- renderPlot({
    ggplot(ev_data, aes(x = Electric_Vehicle_Type, y = Electric_Range)) +
      geom_boxplot(fill = "#1f77b4") +
      labs(title = "Box Plot of Electric Vehicle Type vs. Electric Range") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # Rotate x-axis labels vertically
  })
  
}

shinyApp(ui, server)


library(shiny)
library(quantmod)
library(plotly)
library(DT)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Stock Data Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      # Input for stock ticker selection
      selectInput("ticker", "Select Stock Ticker:",
                  choices = c("META", "AAPL", "AAPC"),
                  selected = "AAPL"),
      
      # Input for date range selection
      dateRangeInput("dates", "Select Date Range:",
                     start = Sys.Date() - 30, end = Sys.Date()),
      
      # Action button to fetch data
      actionButton("fetch_data", "Show Data & Plot"),
      
      # Option to download the plot
      downloadButton("download_plot", "Download Plot"),
      downloadButton("download_data", "Download Data")
    ),
    
    mainPanel(
      # Plot Output
      plotlyOutput("stock_plot"),
      
      # Data Table Output
      DTOutput("stock_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to fetch stock data when button is pressed
  stock_data <- reactive({
    req(input$fetch_data)
    
    # Fetch stock data from Yahoo Finance
    data <- getSymbols(input$ticker, src = "yahoo", 
                       from = input$dates[1], to = input$dates[2], 
                       auto.assign = FALSE)
    
    # Manipulate and clean data with dplyr
    stock_df <- data.frame(Date = index(data),
                           Ticker = rep(input$ticker, nrow(data)),
                           Closing_Price = as.numeric(Cl(data)),
                           Open = as.numeric(Op(data)),
                           High = as.numeric(Hi(data)),
                           Low = as.numeric(Lo(data)),
                           Volume = as.numeric(Vo(data)))
    
    # Create additional features (e.g., moving average)
    stock_df <- stock_df %>%
      mutate(
        Moving_Avg_20 = zoo::rollmean(Closing_Price, 20, fill = NA, align = "right"),
        Moving_Avg_50 = zoo::rollmean(Closing_Price, 50, fill = NA, align = "right")
      )
    
    return(stock_df)
  })
  
  # Render stock plot using Plotly
  output$stock_plot <- renderPlotly({
    stock_df <- stock_data()
    
    # Create plot with moving averages and closing price
    plot_ly(stock_df, x = ~Date, y = ~Closing_Price, type = 'scatter', mode = 'lines', name = "Closing Price") %>%
      add_trace(y = ~Moving_Avg_20, mode = 'lines', name = "20-Day Moving Average", line = list(dash = 'dash', color = 'blue')) %>%
      add_trace(y = ~Moving_Avg_50, mode = 'lines', name = "50-Day Moving Average", line = list(dash = 'dash', color = 'red')) %>%
      layout(
        title = paste(input$ticker, "Stock Price with Moving Averages"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price (USD)")
      )
  })
  
  # Render stock data table using DT
  output$stock_table <- renderDT({
    stock_df <- stock_data()
    
    # Select columns for display
    stock_df <- stock_df %>%
      select(Date, Ticker, Closing_Price, Moving_Avg_20, Moving_Avg_50)
    
    datatable(stock_df, options = list(pageLength = 10))
  })
  
  # Handle download of the plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste(input$ticker, "_stock_plot.png", sep = "")
    },
    content = function(file) {
      stock_df <- stock_data()
      plot <- plot_ly(stock_df, x = ~Date, y = ~Closing_Price, type = 'scatter', mode = 'lines', name = "Closing Price") %>%
        add_trace(y = ~Moving_Avg_20, mode = 'lines', name = "20-Day Moving Average", line = list(dash = 'dash', color = 'blue')) %>%
        add_trace(y = ~Moving_Avg_50, mode = 'lines', name = "50-Day Moving Average", line = list(dash = 'dash', color = 'red')) %>%
        layout(
          title = paste(input$ticker, "Stock Price with Moving Averages"),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Price (USD)")
        )
      # Save the plot as PNG
      export::graph2png(plot, file = file)
    }
  )
  
  # Handle download of the data
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$ticker, "_stock_data.csv", sep = "")
    },
    content = function(file) {
      stock_df <- stock_data()
      # Write the manipulated data to a CSV file
      write.csv(stock_df, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

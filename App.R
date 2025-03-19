library(shiny)
library(bslib)
library(plotly)
library(highcharter)
library(leaflet)
library(DT)
library(reactable)
library(lubridate)
library(tidyverse)
library(rio)


retail_data <- rio::import("~/RStudio/online_retail_dashboard/data/retail_data.rds", trust = TRUE) |> 
  as_tibble()

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly" # theme
    ),
  navset_card_underline(
      title = h3("Retail Data Dashboard"),
      sidebar = sidebar(
        selectInput("fiscalyear",
                    "Select the fiscal year:",
                    choices = c(2010, 2011)
                    ),
        selectInput("country",
                    "Select the Country:",
                    choices = sort(unique(retail_data$Country)))
        )
        ),
      nav_panel("Sales Trend", plotlyOutput("salesPlot")),
      nav_panel("Top Products", highchartOutput("productChart")),
      nav_panel("Sales Map", leafletOutput("salesMap")),
      nav_panel("Transactions", DTOutput("salesTable")),
      nav_panel("Top Customers", reactableOutput("customerTable")),
      nav_spacer(),
      nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
      )
  )


server <- function(input, output, session) {
  # Sales Trend
  output$salesPlot <- renderPlotly({
    sales_trend <- retail_data %>%
      group_by(InvoiceDate) %>%
      summarize(TotalRevenue = sum(Revenue, na.rm = TRUE))
    plot_ly(sales_trend, x = ~InvoiceDate, y = ~TotalRevenue, type = 'scatter', mode = 'lines')
  })
}

shinyApp(ui, server)
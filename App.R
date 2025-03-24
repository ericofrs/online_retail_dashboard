library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(bslib)
library(plotly)
library(highcharter)
library(leaflet)
library(leaflet.extras)
library(DT)
library(reactable)
library(lubridate)
library(tidyverse)
library(rio)


retail_data <- rio::import("~/RStudio/online_retail_dashboard/data/retail_data.rds", trust = TRUE) |> 
  as_tibble()

retail_w_coord <- rio::import("~/RStudio/online_retail_dashboard/data/retail_w_coord.rds", trust = TRUE) |> 
  as_tibble()

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly", # theme
    base_font = font_google("Titillium Web")
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
                    choices = sort(unique(retail_data$Country))
                    ),
        br(),
        useShinyjs(),
        radioGroupButtons(inputId = "file_format", 
                          label = "Select data format:",
                          choices = c("CSV (.csv)" = ".csv",
                                      "Excel (.xlsx)" = ".xlsx"),
                          justified = TRUE),
        downloadBttn(outputId = "download_data", 
                     label = "Download the Data",
                     style = "fill",
                     color = "primary"),
        div(id = "download_spinner", 
              style = "display: none; text-align: center; margin-top: 10px;",
              tags$img(src = "spinner.gif", height = "50px")
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
  #Download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("retail_data", input$file_format, sep = "")
    },
    content = function(file) {
      runjs("$('#download_spinner').show();")
      Sys.sleep(3)
      req(retail_data)
      rio::export(x=retail_data,file = file)
      
      runjs("$('#download_spinner').hide();")
    }
  )
  
  # Sales Trend
  output$salesPlot <- renderPlotly({
    sales_trend <- retail_data |> 
      group_by(InvoiceDate) |> 
      summarize(TotalRevenue = sum(Revenue, na.rm = TRUE))
    plot_ly(sales_trend, x = ~InvoiceDate, y = ~TotalRevenue,
            type = 'scatter', 
            mode = 'lines')
  })
  
  # Top Selling products
  output$productChart <- renderHighchart({
    top_products <- retail_data  |> 
      group_by(StockCode, Description) |>
      summarize(TotalRevenue = sum(Revenue, na.rm = TRUE),
                TotalQuantity = sum(Quantity, na.rm = TRUE)) |>
      arrange(desc(TotalRevenue)) |>
      head(10)
    
    highchart() |>
      hc_chart(type = "column") |>
      hc_xAxis(categories = top_products$Description,
               crosshair = T) |>
      hc_add_series(name = "Quantity", data = top_products$TotalQuantity, type = "column") |>
      hc_add_series(name = "Revenue", data = top_products$TotalRevenue, type = "spline", yAxis = 1) |>
      hc_yAxis_multiples(
        list(title = list(text = "Quantity"), opposite = FALSE),
        list(title = list(text = "Revenue"), opposite = TRUE)
      ) |>
      hc_tooltip(shared = TRUE,
                 valueDecimals = 0)
  })
  
  # Sales Map
  country_sales <- retail_w_coord |> 
    group_by(Country, Lat, Lon) |> 
    summarize(TotalRevenue = sum(Revenue, na.rm = TRUE))
  
  output$salesMap <- renderLeaflet({
    leaflet(country_sales) |> 
      addProviderTiles(providers$CartoDB.Positron, group = "PositronLight") |> 
      addProviderTiles(providers$Stadia.AlidadeSmoothDark, group = "AlidadeDark") |> 
      addLayersControl(
        baseGroups = c(
          "PositronLight", "AlidadeDark"),
        position = "bottomleft"
      ) |> 
      addMarkers(~Lon, ~Lat, popup = ~paste(Country, "Sales:", TotalRevenue))
  })
  
  # Sales Table
  output$salesTable <- renderDT({
    datatable(retail_data, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # Top Customers Table
  output$customerTable <- renderReactable({
    top_customers <- retail_data %>%
      group_by(CustomerID) %>%
      summarize(TotalSpent = sum(Revenue, na.rm = TRUE)) %>%
      arrange(desc(TotalSpent)) %>%
      head(10)
    
    reactable(top_customers, 
              columns = list(
                TotalSpent = colDef(style = function(value) {
                  if (value > 150000) "color: darkblue; font-weight: bold;" else NULL
                })
              ),
              # searchable = TRUE,
              filterable = TRUE,
              sortable = TRUE
    )
  })
  
}

shinyApp(ui, server)
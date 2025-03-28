library(shiny)
library(shinyjs)
library(shinycssloaders)
library(waiter)
library(shinyalert)
library(rmarkdown)
library(shinyWidgets)
library(shinymanager)
library(keyring)
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

# Define UI
## page_sidebar
ui <- page_sidebar(
  useWaitress(),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly" # theme
    # base_font = font_google("Titillium Web")
    ),
  title = h3("Retail Data Dashboard"),
  sidebar = sidebar(
    virtualSelectInput(inputId = "fiscalyear",
                      label = "Select the fiscal year(s):", 
                      choices = unique(retail_data$FiscalYear),
                      selected = unique(retail_data$FiscalYear),
                      multiple = TRUE,
                      showValueAsTags = TRUE,
                      width = "100%",
                      dropboxWrapper = "body", 
                      required = TRUE,
                      minValues = 1
                      ),
    pickerInput(inputId = "country",
                label = "Select the Country:",
                choices = sort(unique(retail_data$Country)),
                options = pickerOptions(container = "body",
                                        width = "100%",
                                        liveSearch = TRUE)
                ),
    actionBttn(inputId = "apply_filter",
               label = "Apply Filters",
               style = "stretch",
               color = "primary"),
    br(),
    useShinyjs(),
    radioGroupButtons(inputId = "file_format", 
                      label = "Select data format:",
                      choices = c("CSV (.csv)" = ".csv",
                                  "Excel (.xlsx)" = ".xlsx"),
                      justified = TRUE,
                      size = "sm"),
    downloadBttn(outputId = "download_data", 
                 label = "Download the Data",
                 style = "fill",
                 color = "primary",
                 size = "sm"),
    downloadBttn(outputId = "download_report", 
                 label = "Download the Report",
                 style = "fill",
                 color = "primary",
                 size = "sm"),
    div(id = "download_spinner", 
        style = "display: none; text-align: center; margin-top: 10px;",
        tags$img(src = "spinner.gif", height = "50px")
    )
  ),
  navset_card_underline(
      nav_panel("Sales Trend", plotlyOutput("salesPlot")),
      nav_panel("Top Products", highchartOutput("productChart")),
      nav_panel("Sales Map", leafletOutput("salesMap")),
      nav_panel("Transactions", DTOutput("salesTable")),
      nav_panel("Top Customers", reactableOutput("customerTable")),
      nav_spacer(),
      nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
      )
)

# # Set the secure_app
# ui <- secure_app(ui,
#                  theme = bs_theme(
#                    version = 5,
#                    bootswatch = "flatly"),
#                  fab_position = "top-right",
#                  enable_admin = TRUE)

#Define server
server <- function(input, output, session) {
  
  # browser()
  
  # #Check the user
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(
  #     "data/users.sqlite",
  #     passphrase = keyring::key_get("R-shinymanager-key", Sys.getenv("shinymanager_passphrase"))
  #   )
  #   )
  
  # call the waitress
  waitress <- Waitress$
    new(theme = "overlay-percent")$
    start() # start
    for(i in 1:10){
    waitress$inc(10) # increase by 10%
    Sys.sleep(.3)
  }
    # hide when it's done
  waitress$close()
  
  # filtered_data <- reactive({
  #   req(input$fiscalyear, input$country)
  #   
  #   retail_data |>
  #     filter(FiscalYear %in% input$fiscalyear, 
  #            Country %in% input$country)
  # })
  
  filtered_data <- reactiveVal(retail_data)
  country_sales_reactive <- reactiveVal({
    retail_w_coord |> 
      group_by(Country, Lat, Lon) |> 
      summarize(TotalRevenue = sum(Revenue, na.rm = TRUE))
  })
  
  observeEvent(input$apply_filter, {
    if (length(input$fiscalyear) == 0 || length(input$country) == 0) {
      shinyalert(title = "Attention!", 
                 text = "Please select at least one year and one country", 
                 type = "error",
                 size = "m", 
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 showConfirmButton = TRUE,
                 showCancelButton = FALSE)
    } else {
      filtered_data(
        retail_data |>
          filter(FiscalYear %in% input$fiscalyear, 
                 Country %in% input$country)
      )
      
      country_sales_reactive( 
        retail_w_coord |> 
          filter(FiscalYear %in% input$fiscalyear) |> 
          group_by(Country, Lat, Lon) |> 
          summarize(TotalRevenue = sum(Revenue, na.rm = TRUE))
      )
    }
  })
  
  #Download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("retail_data", input$file_format, sep = "")
    },
    content = function(file) {
      runjs("$('#download_spinner').show();")
      Sys.sleep(3)
      req(filtered_data())
      rio::export(x=filtered_data(),file = file)
      runjs("$('#download_spinner').hide();")
      shinyalert(title = "Congrats", 
                 text = "The Data has been downloaded", 
                 type = "success",
                 size = "xs", 
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 showConfirmButton = FALSE,
                 showCancelButton = FALSE,
                 timer = 2000)
    }
  )
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("report-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Check if data has been filtered
      if (nrow(filtered_data()) == nrow(retail_data)) {
        # If no filtering has occurred, show an alert
        shinyalert(
          title = "Filter the data first", 
          text = "Please apply filters before downloading the report", 
          type = "warning",
          size = "m", 
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          showConfirmButton = TRUE,
          showCancelButton = FALSE
        )
      } else {
        runjs("$('#download_spinner').show();")
        Sys.sleep(3)
        
        rmarkdown::render(
          input = "www/template.Rmd",
          output_file = file,
          params = list(
            data = filtered_data(),
            country = input$country,
            years = input$fiscalyear
          ),
          envir = new.env(parent = globalenv())
        )
        runjs("$('#download_spinner').hide();")
        shinyalert(
          title = "Great", 
          text = "The Report has been downloaded", 
          type = "success",
          size = "xs", 
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          showConfirmButton = FALSE,
          showCancelButton = FALSE,
          timer = 2000
        )
      }
    }
  )
  
  # Sales Trend
  output$salesPlot <- renderPlotly({
    sales_trend <- filtered_data() |> 
      group_by(InvoiceDate) |> 
      summarize(TotalRevenue = sum(Revenue, na.rm = TRUE))
    
    plot_ly(sales_trend, x = ~InvoiceDate, y = ~TotalRevenue,
            type = 'scatter', 
            mode = 'lines') |>
      layout(
        title = "Sales Trend",
        xaxis = list(title = "Invoice Date"),
        yaxis = list(title = "Total Revenue")
      )
  })
  
  # Top Selling products
  output$productChart <- renderHighchart({
    top_products <- filtered_data()  |> 
      group_by(StockCode, Description) |>
      summarize(TotalRevenue = sum(Revenue, na.rm = TRUE),
                TotalQuantity = sum(Quantity, na.rm = TRUE)) |>
      arrange(desc(TotalRevenue)) |>
      head(10)
    
    highchart() |>
      hc_chart(type = "column") |>
      hc_xAxis(categories = top_products$Description,
               crosshair = TRUE) |>
      hc_add_series(name = "Quantity", data = top_products$TotalQuantity, type = "column") |>
      hc_add_series(name = "Revenue", data = top_products$TotalRevenue, type = "spline", yAxis = 1) |>
      hc_yAxis_multiples(
        list(title = list(text = "Quantity"), opposite = FALSE),
        list(title = list(text = "Revenue"), opposite = TRUE)
      ) |>
      hc_tooltip(shared = TRUE,
                 valueDecimals = 0) |>
      hc_title(text = "Top 10 Selling Products")
  })
  
  # Sales Map
  output$salesMap <- renderLeaflet({
    country_sales <- country_sales_reactive()
    marker_colors <- ifelse(
      country_sales$Country %in% input$country, 
      "green",  # Selected countries in green
      "blue"  # Other countries in blue
    )
    leaflet(country_sales) |> 
      addProviderTiles(providers$CartoDB.Positron, group = "PositronLight") |> 
      addProviderTiles(providers$Stadia.AlidadeSmoothDark, group = "AlidadeDark") |> 
      addLayersControl(
        baseGroups = c(
          "PositronLight", "AlidadeDark"),
        position = "bottomleft"
      ) |> 
      addCircleMarkers(~Lon, ~Lat, 
                       color = marker_colors,
                       radius = 8,
                       popup = ~paste(Country, "Sales:", round(TotalRevenue,2)))
  })
  
  # Sales Table
  output$salesTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # Top Customers Table
  output$customerTable <- renderReactable({
    top_customers <- filtered_data() %>%
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
              filterable = TRUE,
              sortable = TRUE
    )
  })
  
}

shinyApp(ui, server)
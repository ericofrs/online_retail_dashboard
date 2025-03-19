library(dplyr)
library(rio)

data2010 <- import("~/RStudio/online_retail_dashboard/data/online_retail_2010.csv")
data2011 <- import("~/RStudio/online_retail_dashboard/data/online_retail_2011.csv")

retail_data <- rbind(data2010,data2011)

glimpse(retail_data)
summary(retail_data)

retail_data <- retail_data |>
  as_tibble() |>
  rename(CustomerID = `Customer ID`) |>
  filter(!is.na(CustomerID)) |>
  mutate(
    Country = as.factor(Country),
    StockCode = as.factor(StockCode),
    Description = factor(Description, levels = unique(Description[order(StockCode)])),
    InvoiceDate = as.Date(dmy_hm(InvoiceDate)),
    Revenue = Quantity * Price
  )

rio::export(retail_data, "~/RStudio/online_retail_dashboard/data/retail_data.rds")

library(dplyr)
library(rio)

data2010 <- import("~/RStudio/online_retail_dashboard/data/online_retail_2010.csv")
data2011 <- import("~/RStudio/online_retail_dashboard/data/online_retail_2011.csv")

retail_data <- rbind(data2010,data2011)

glimpse(retail_data)
summary(retail_data)

# Main data file
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

# File with geo coord.
country_coords <- data.frame(
  Country   = c("Australia", "Austria", "Bahrain", "Belgium", "Brazil", "Canada", 
                "Channel Islands", "Cyprus", "Czech Republic", "Denmark", "EIRE", 
                "European Community", "Finland", "France", "Germany", "Greece", 
                "Iceland", "Israel", "Italy", "Japan", "Korea", "Lebanon", 
                "Lithuania", "Malta", "Netherlands", "Nigeria", "Norway", "Poland", 
                "Portugal", "RSA", "Saudi Arabia", "Singapore", "Spain", "Sweden", 
                "Switzerland", "Thailand", "United Arab Emirates", "United Kingdom", 
                "Unspecified", "USA", "West Indies"),
  Lat = c(-25.2744, 47.5162, 26.0667, 50.8503, -14.2350, 45.4215, 
          49.3723, 35.1264, 49.8175, 56.2639, 53.1424, 
          50.8503, 61.9241, 48.8566, 52.5200, 37.9838, 
          64.9631, 31.0461, 41.8719, 36.2048, 35.9078, 33.8547, 
          55.1694, 35.9375, 52.3676, 9.0820, 60.4720, 51.9194, 
          39.3999, -30.5595, 23.8859, 1.3521, 40.4637, 60.1282, 
          46.8182, 15.8700, 23.4241, 51.509865, 0.0000, 37.7749, 18.1096),
  Lon = c(133.7751, 14.5501, 50.5577, 4.3517, -51.9253, -75.6972, 
          -2.3644, 33.4299, 15.4725, 9.5018, -7.6921, 
          4.3517, 25.7482, 2.3522, 13.4050, 23.7275, 
          -19.0208, 34.8516, 12.5674, 138.2529, 127.7669, 35.8623, 
          23.8813, 14.3754, 4.9041, 8.6753, 8.4689, 19.1451, 
          -8.2245, 22.9375, 45.0792, 103.8198, -3.7492, 18.6435, 
          8.2275, 100.9925, 53.8478, -0.118092, -30.0000, -122.4194, -64.8963)
)

retail_w_coord <- merge(retail_data, country_coords, by = "Country", all.x = TRUE)

rio::export(retail_w_coord, "~/RStudio/online_retail_dashboard/data/retail_w_coord.rds")


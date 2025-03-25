library(dplyr)
library(rio)

data2010 <- import("~/RStudio/online_retail_dashboard/data/online_retail_2010.csv") |> 
  mutate(FiscalYear = factor(2010))

data2011 <- import("~/RStudio/online_retail_dashboard/data/online_retail_2011.csv") |> 
  mutate(FiscalYear = factor(2011))

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
  Country = c("Australia", "Austria", "Bahrain", "Belgium", "Brazil", "Canada", 
              "Channel Islands", "Cyprus", "Czech Republic", "Denmark", "EIRE", 
              "European Community", "Finland", "France", "Germany", "Greece", 
              "Iceland", "Israel", "Italy", "Japan", "Korea", "Lebanon", 
              "Lithuania", "Malta", "Netherlands", "Nigeria", "Norway", "Poland", 
              "Portugal", "RSA", "Saudi Arabia", "Singapore", "Spain", "Sweden", 
              "Switzerland", "Thailand", "United Arab Emirates", "United Kingdom", 
              "Unspecified", "USA", "West Indies"),
  Capital = c("Canberra", "Vienna", "Manama", "Brussels", "Brasília", "Ottawa", 
              "Saint Helier", "Nicosia", "Prague", "Copenhagen", "Dublin", 
              "Strasbourg", "Helsinki", "Paris", "Berlin", "Athens", 
              "Reykjavik", "Jerusalem", "Rome", "Tokyo", "Seoul", "Beirut", 
              "Vilnius", "Valletta", "Amsterdam", "Abuja", "Oslo", "Warsaw", 
              "Lisbon", "Pretoria", "Riyadh", "Singapore", "Madrid", "Stockholm", 
              "Bern", "Bangkok", "Abu Dhabi", "London", "Mid-Atlantic", "Washington, D.C.", "Kingston"),
  Lat = c(-35.2820, 48.2082, 26.2285, 50.8503, -15.7801, 45.4215, 
          49.3723, 35.1856, 50.0755, 55.6761, 53.3498, 
          48.5734, 60.1695, 48.8566, 52.5200, 37.9838, 
          64.1355, 31.7683, 41.9028, 35.6895, 37.5665, 33.8886, 
          54.6872, 35.8997, 52.3676, 9.0579, 59.9139, 52.2298, 
          38.7169, -25.7461, 24.7136, 1.3521, 40.4168, 59.3293, 
          46.9481, 13.7563, 24.4539, 51.509865, 0.0000, 38.9072, 17.5707),
  Lon = c(149.1286, 16.3738, 50.5860, 4.3517, -47.9292, -75.6972, 
          -2.3644, 33.3823, 14.4378, 12.5683, -6.2603, 
          7.7521, 24.9354, 2.3522, 13.4050, 23.7275, 
          -21.8954, 35.2137, 12.4964, 139.6917, 126.9780, 35.4955, 
          25.2797, 14.5146, 4.9041, 7.4898, 10.7522, 21.0122, 
          -9.1399, 28.1876, 46.6753, 103.8198, -3.7038, 18.0686, 
          7.4474, 100.5018, 54.3776, -0.118092, -30.0000, -77.0369, -61.4558)
)


retail_w_coord <- merge(retail_data, country_coords, by = "Country", all.x = TRUE)

rio::export(retail_w_coord, "~/RStudio/online_retail_dashboard/data/retail_w_coord.rds")


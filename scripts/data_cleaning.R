library(dplyr)
library(rio)

data2010 <- import("~/RStudio/online_retail_dashboard/data/online_retail_2010.csv")
data2011 <- import("~/RStudio/online_retail_dashboard/data/online_retail_2011.csv")

retail_data <- rbind(data2010,data2011)


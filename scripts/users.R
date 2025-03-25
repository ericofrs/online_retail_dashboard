
credentials <- data.frame(
  user = c(Sys.getenv("user1_login"), Sys.getenv("user2_login")), # mandatory
  password = c(Sys.getenv("user1_pw"), Sys.getenv("user2_pw")), # mandatory
  start = c("2025-03-25"),
  expire = c(NA, NA),
  admin = c(Sys.getenv("admin1"), Sys.getenv("admin2"))
  # comment = "Simple and secure authentification mechanism for single ‘Shiny’ applications.",
  # stringsAsFactors = FALSE
)

rio::export(credentials, "~/RStudio/online_retail_dashboard/data/credentials.csv")
library(DBI)
library(RSQLite)


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


conn <- DBI::dbConnect(RSQLite::SQLite(), "~/RStudio/online_retail_dashboard/data/mydatabase.db")

query_users <- "SELECT * FROM users"
users_data <- dbGetQuery(conn, query_users)

query_deps <- "SELECT * FROM departments"
deps_data <- dbGetQuery(conn, query_deps)

insert_deps <- "INSERT INTO departments (Department, Name, Username)
  VALUES
    ('HR', 'Big Boss', 'admin'),
    ('IT', 'Robert Smith', 'bob'),
    ('Sales', 'Carl Johnson', 'carl'),
    ('Sales', 'Jane Williams', 'jane'),
    ('IT', 'Newbie', 'new_user');"


dbExecute(conn, insert_deps)

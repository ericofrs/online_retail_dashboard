library(DBI)
library(RSQLite)

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

# Example on how to tweak the R vector so it can be used in a query
# username_vector <- c("carlos", "pedro")
# username_string <- paste0("'", username_vector, "'", collapse = ", ")
# query <- paste0("SELECT * FROM users WHERE username IN (", username_string, ")")


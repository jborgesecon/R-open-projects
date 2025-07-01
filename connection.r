# libs
library(DBI)
library(RPostgres)
library(dotenv)
library(readr)
library(dplyr)

# # conn
# Set up connection (example with PostgreSQL)
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("DB_NAME"),
                 host = Sys.getenv("DB_HOST"),
                 port = as.integer(Sys.getenv("DB_PORT")),
                 user = Sys.getenv("DB_USER"),
                 password = Sys.getenv("DB_PASSWORD"))

# Helper function to get query
execute_sql <- function(filename) {
  sql_query <- read_file(glue("ewz_bvsp/queries/{filename}.sql"))
  result <- dbGetQuery(con, sql_query)
  dbDisconnect(con)
  return(result)
}

# Helper function to feed database
feed_db <- function(df, schema, tablename) {
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id(schema = schema, table = tablename),
    value = df,
    overwrite = TRUE
  )
}

# install.packages("RSQLite")
library(RSQLite)
library(readr)
mydb <- dbConnect(RSQLite::SQLite(), "my-db.sqlite")

sql_file <- "~/Documents/rodm2/ODM2/src/blank_schema_scripts/sqlite/ODM2_for_SQLite.sql"
# put sql files for schema creation in inst folder

# https://stackoverflow.com/questions/18914283/how-to-execute-more-than-one-rsqlite-statement-at-once-or-how-to-dump-a-whole-fi
#
# read in sql-statements and preformat them
sqlFromFile <- function(file){
  require(stringr)
  sql <- readLines(file)
  sql <- unlist(str_split(paste(sql,collapse=" "),";"))
  sql <- sql[grep("^ *$", sql, invert=T)]
  sql
}

# apply query function to each element
dbSendQueries <- function(con,sql){
  dummyfunction <- function(sql,con){
    dbSendQuery(con,sql)
  }
  lapply(sql, dummyfunction, con)
}

# solution for example in question
dbSendQueries( mydb, sqlFromFile(sql_file) )

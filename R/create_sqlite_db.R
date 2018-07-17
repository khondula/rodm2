
#' Make a new ODM2 sqlite database
#'
#' @param dir Name of existing directory for the sqlite database file to be created in. Defaults to current directory
#' @param filename Filename for sqlite database file. Defaults to `odm2`, creating odm2.sqlite
#' @param connect whether or not to return the database connection object
#'
#' @details This function copies a template sqlite file with the ODM2 schema and controlled vocabularies to the specified location.
#'
#' @export
#'
#' @examples
#' create_sqlite(dir = tempdir())
#' dblite <- DBI::dbConnect(RSQLite::SQLite(), "odm2.sqlite")

create_sqlite <- function(dir = ".", filename = "odm2", connect = FALSE){
  path <- file.path(dir)
  template_file <- system.file("odm2-template.sqlite", package = "rodm2")
  file.copy(template_file, path)
  file.rename(file.path(dir, "odm2-template.sqlite"), file.path(dir, paste0(filename, ".sqlite")))
  invisible(template_file)
  if(connect){
    return(DBI::dbConnect(RSQLite::SQLite(),
                          dbname = file.path(dir, paste0(filename, ".sqlite"))))
}
}





# install.packages("RSQLite")
# library(RSQLite)
# library(readr)
# library(DBI)
# mydb <- dbConnect(RSQLite::SQLite(), "my-db.sqlite")
#
# SQL(readLines(sql_file))
#
# sql_file <- "~/Documents/rodm2/ODM2/src/blank_schema_scripts/sqlite/ODM2_for_SQLite.sql"
# # put sql files for schema creation in inst folder
#
# # https://stackoverflow.com/questions/18914283/how-to-execute-more-than-one-rsqlite-statement-at-once-or-how-to-dump-a-whole-fi
# #
# # read in sql-statements and preformat them
# sqlFromFile <- function(file){
#   require(stringr)
#   sql <- readLines(file)
#   sql <- unlist(str_split(paste(sql,collapse=" "),";"))
#   sql <- sql[grep("^ *$", sql, invert=T)]
#   sql
# }
#
# # apply query function to each element
# dbSendQueries <- function(con,sql){
#   dummyfunction <- function(sql,con){
#     dbSendQuery(con,sql)
#   }
#   lapply(sql, dummyfunction, con)
# }
#
# # solution for example in question
# dbSendQueries(mydb, sqlFromFile(sql_file))
#
# # populate controlled vocabulary tables
#
# # run in terminal...
# system("python inst/cvload.py sqlite:///my-db.sqlite")
# dbListTables(mydb)
# cv_specimenType <- dbReadTable(mydb, "CV_SpecimenType")
# # definitions are not coming through (does it matter?)
# names(units) <- dbListFields(mydb, "units")[c(4, 3, 2)]
#
# dbListFields(mydb, "units")
# dbWriteTable(mydb, "units",
#              units,
#              overwrite = FALSE,
#              row.names = FALSE,
#              append = TRUE)

# save as 'blank' odm2 sqlite


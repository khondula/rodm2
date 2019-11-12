
#' Make a new ODM2 sqlite database
#'
#' @param dir Name of existing directory for the sqlite database file to be created in. Defaults to current directory
#' @param filename Filename for sqlite database file. Defaults to "odm2", making a file called odm2.sqlite
#' @param connect Whether or not to return the database connection object
#' @return Returns database connecton object if connect = TRUE, otherwise just creates file.
#'
#' @details This function copies a template sqlite file with the ODM2 schema, controlled vocabularies,
#' and units to the specified location.
#' @export
#'
#' @examples
#' # create_sqlite()
#' # db <- connect_sqlite()

create_sqlite <- function(dir = ".", filename = "odm2.sqlite", connect = FALSE){
  path <- file.path(dir)

  # if(file.exists(file.path(dir, paste0(filename, ".sqlite")))){
  #   message("File already exists. Use connect_sqlite() to connect to existing sqlite database.")
  # }

  template_file <- system.file("odm2-template.sqlite", package = "rodm2")
  file.copy(template_file, path)
  file.rename(file.path(dir, "odm2-template.sqlite"), file.path(dir, filename))
  invisible(template_file)
  if(connect){
    return(DBI::dbConnect(RSQLite::SQLite(),
                          dbname = file.path(dir, filename)))
}
}

#' Connect to an existing ODM2 sqlite database
#'
#' @param filename Filename for sqlite database file. Defaults to "odm2"
#' @param dir Name of existing directory where database file is stored. Defaults to current working directory
#'
#' @details This function returns a database connection object to be used in subsequent inserts and queries.
#' @return A database connection object
#' @export
#'
#' @examples
#' # create_sqlite(filename = "odm2", dir = tempdir())
#' # db <- connect_sqlite(filename = "odm2.sqlite")
connect_sqlite <- function(filename = "odm2.sqlite", dir = "."){

  path <- file.path(dir, filename)
  conn_obj <- DBI::dbConnect(RSQLite::SQLite(), path)

  return(conn_obj)
}

#' Disconnect from connect ODM2 sqlite database
#'
#' @param db database connection object
#'
#' @details This function is a simple wrapper around dbDisconnect
#' @return A message about how to reconnect
#' @export
#'
#' @examples
#' db <- create_sqlite(filename = "odm2.sqlite", dir = tempdir(), connect = TRUE)
#' disconnect_sqlite(db)
#'
disconnect_sqlite <- function(db = db){
  db_filename <- basename(db@dbname)
  DBI::dbDisconnect(db)

  message(glue::glue("Database connection closed.\nReconnect using connect_sqlite(\"{db_filename}\") or create a new database with create_sqlite()."))
}


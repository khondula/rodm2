
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
#' create_sqlite(dir = tempdir())
#' dblite <- create_sqlite(connect = TRUE)

create_sqlite <- function(dir = ".", filename = "odm2", connect = FALSE){
  path <- file.path(dir)

  if(file.exists(file.path(dir, paste0(filename, ".sqlite")))){
    stop("File already exists. Use connect_sqlite() to connect to existing sqlite database.")
  }

  template_file <- system.file("odm2-template.sqlite", package = "rodm2")
  file.copy(template_file, path)
  file.rename(file.path(dir, "odm2-template.sqlite"), file.path(dir, paste0(filename, ".sqlite")))
  invisible(template_file)
  if(connect){
    return(DBI::dbConnect(RSQLite::SQLite(),
                          dbname = file.path(dir, paste0(filename, ".sqlite"))))
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
#' create_sqlite(filename = "odm2", dir = tempdir())
#' db <- connect_sqlite(filename = "odm2')
connect_sqlite <- function(filename = "odm2", dir = "."){

  path <- file.path(dir, paste0(filename, ".sqlite"))
  conn_obj <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  # check if it has same tables as template file
  conn_tables <- RSQLite::dbListTables(conn_obj)

  template_file <- system.file("odm2-template.sqlite", package = "rodm2")
  db_template <- DBI::dbConnect(RSQLite::SQLite(), template_file)
  odm2_tables <- RSQLite::dbListTables(db_template)
  DBI::dbDisconnect(db_template)
  if(!setequal(odm2_tables, conn_tables)){
    warning("This file does not match the ODM2 schema.")
  }

  return(conn_obj)
}


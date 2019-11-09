#' Add a new entry to the Organizations table
#'
#' @param db database connection object to sqlite or postgresql database with odm2 schema
#' @param OrganizationTypeCV Controlled vocab term defining the type of organization (e.g., government agency, university, etc.)
#' @param OrganizationCode A text code identifying the Organization (e.g., USGS)
#' @param OrganizationName The full text name of the organization
#'
#' @return message that your entry has been added
#' @export
#'
#' @examples
#' # connect_sqlite(dir = ".")
#' # db <- DBI::dbConnect(RSQLite::SQLite(), "odm2.sqlite")
#' # db_describe_organization(db, "Manufacturer", "Onset", "Onset")
db_describe_organization <- function(db = db,
                                     OrganizationTypeCV,
                                     OrganizationCode,
                                     OrganizationName){
  # add check to see if person already exists in database
  # and ask to continue or update information
  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}

  # check type of database object
  if (class(db) == "SQLiteConnection"){

    sql1 <- RSQLite::dbSendStatement(db,
                                     'INSERT into organizations
                                     (OrganizationTypeCV, OrganizationCode, OrganizationName)
                                     VALUES
                                     (:OrganizationTypeCV, :OrganizationCode, :OrganizationName)')
    RSQLite::dbBind(sql1, param = list(OrganizationTypeCV = OrganizationTypeCV,
                                       OrganizationCode = OrganizationCode,
                                       OrganizationName = OrganizationName))
    RSQLite::dbClearResult(res = sql1)

    message(paste(OrganizationTypeCV, OrganizationName,
                  "has been entered into the Organizations table."))

  }

  if (class(db) == "PostgreSQLConnection"){

    sql <- DBI::sqlInterpolate(db,
                               'INSERT into odm2.organizations
                                (OrganizationTypeCV, OrganizationCode, OrganizationName)
                               VALUES (?OrganizationTypeCV, ?OrganizationCode, ?OrganizationName)',
                               OrganizationTypeCV = OrganizationTypeCV,
                               OrganizationCode = OrganizationCode,
                               OrganizationName = OrganizationName)

    RPostgreSQL::dbGetQuery(db, sql)
    message(paste(OrganizationTypeCV, OrganizationName,
                  "has been entered into the Organizations table."))

  }


}

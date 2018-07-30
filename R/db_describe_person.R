
#' Describe a new person in the people and affiliations tables
#'
#' @param PersonFirstName First name of the person
#' @param PersonLastName Last name of the person
#' @param AffiliationStartDate The date (YYYY-MM-DD) on which the person became affiliated with the organization. Default is todays date
#' @param PrimaryEmail The primary email address of the person
#' @param db Database connection object (must be postgresql or sqlite)
#'
#' @return message that person was entered into database
#' @export
#'
#' @examples
#' create_sqlite(dir = ".")
#' db <- DBI::dbConnect(RSQLite::SQLite(), "odm2.sqlite")
#' db_describe_person(db = db, PersonFirstName = "Wendy",
#'     PersonLastName = "Wetland",
#'     AffiliationStartDate = "2018-01-01",
#'     PrimaryEmail = "wendy 'at' swamps.edu")
db_describe_person <- function(db = db,
                               PersonFirstName,
                               PersonLastName,
                               AffiliationStartDate = Sys.Date(),
                               PrimaryEmail){

  # add check to see if person already exists in database
  # and ask to continue or update information
  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}

  # check type of database object
  if (class(db) == "SQLiteConnection"){

    sql1 <- RSQLite::dbSendStatement(db,
                            'INSERT into people
                            (PersonFirstName, PersonLastName)
                            VALUES
                            (:PersonFirstName, :PersonLastName)')
    RSQLite::dbBind(sql1, param = list(PersonFirstName = PersonFirstName,
                              PersonLastName = PersonLastName))
    RSQLite::dbClearResult(res = sql1)
    sql2 <- RSQLite::dbSendStatement(db,
                            'INSERT into affiliations
                            (PersonID, AffiliationStartDate, PrimaryEmail)
                            VALUES (
                            (SELECT PersonID FROM People WHERE PersonFirstName = :PersonFirstName AND PersonLastName = :PersonLastName),
                            :AffiliationStartDate,
                            :PrimaryEmail)')
    RSQLite::dbBind(sql2, param = list(PersonFirstName = PersonFirstName,
                              PersonLastName = PersonLastName,
                              AffiliationStartDate = AffiliationStartDate,
                              PrimaryEmail = PrimaryEmail))

    message(paste(PersonFirstName, PersonLastName, "has been entered into the People table."))

  }

  if (class(db) == "PostgreSQLConnection"){

    sql <- DBI::sqlInterpolate(db,
                          'WITH
                          newpeople AS (
                          INSERT into odm2.people (PersonFirstName, PersonLastName)
                          VALUES (?PersonFirstName, ?PersonLastName)
                          RETURNING PersonID)

                          INSERT into odm2.affiliations (PersonID, AffiliationStartDate, PrimaryEmail)
                          VALUES ((SELECT newpeople.PersonID FROM newpeople), ?AffiliationStartDate, ?PrimaryEmail)
                          RETURNING AffiliationID',
                          PersonFirstName = PersonFirstName,
                          PersonLastName = PersonLastName,
                          AffiliationStartDate = AffiliationStartDate,
                          PrimaryEmail = PrimaryEmail)

    RPostgreSQL::dbGetQuery(db, sql)
    message(paste(PersonFirstName, PersonLastName, "has been entered into the People table."))

  }


}

#' Get list of people currently in database
#'
#' @param db
#'
#' @return the current first and last names in the people table
#' @export
#'
#' @examples
#' #db_get_methods(db)
db_get_people <- function(db){
  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}
  current_ppl <- c()
  if (class(db) == "SQLiteConnection"){
    current_ppl <- DBI::dbGetQuery(db, "SELECT personfirstname, personlastname FROM people")$personfirstname
  }
  if (class(db) == "PostgreSQLConnection"){
    current_ppl <- DBI::dbGetQuery(db, "SELECT personfirstname, personlastname FROM odm2.people")$personfirstname
  }
  return(current_ppl)
}


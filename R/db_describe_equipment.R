#' Add a piece of equipment to the equipment table
#'
#' @param db database connection
#' @param equip_name a unique short code name of the instrument
#' @param serial_no serial number of equipment
#' @param model_name model name of equipment
#' @param vendor organization that sold the product
#' @param owner_first first name of person who owns equipment
#' @param equipment_type
#' type of equipment from \href{http://vocabulary.odm2.org/equipmenttype/}{controlled vocabulary} defaults to sensor
#' @param owner_last last name of person who owns equipment,
#'    required if the person is not in the people table already
#' @param owner_email email of person who owns equipment,
#'    required if the person is not in the people table arelady
#' @param manufacturer company that makes the model of the equipment,
#'    required if the model is not in the equipmentmodels table
#' @param purchase_date date that the equipment was purchased,
#'    defaults to current date
#'
#' @return TRUE if successful
#' @export
#' @family describe functions
#' @examples
#' # db <- create_sqlite()
#' # OR
#' # db <- connect_sqlite()
#' # db_describe_equipment(db, "unit1", "001", "model1",
#' # "vendor name", "Wendy", "Sensor", "Wetland", "ww email", "hobo")
db_describe_equipment <- function(db,
                                  equip_name,
                                  serial_no,
                                  model_name,
                                  vendor,
                                  owner_first,
                                  equipment_type = "Sensor",
                                  owner_last = NULL,
                                  owner_email = NULL,
                                  manufacturer = NULL,
                                  purchase_date = Sys.Date()){

  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}

  # check type of database object
  if (class(db) == "SQLiteConnection"){

  sql1 <- RSQLite::dbSendStatement(db,
                                   "SELECT equipmentmodelid FROM equipmentmodels WHERE modelname = :modelname")
  RSQLite::dbBind(sql1, param = list(modelname = model_name))
  modelid <- DBI::dbFetch(sql1)
  RSQLite::dbClearResult(sql1)
  # check if the model is not already in models table
  if(nrow(modelid) < 1){
    # if not, check that manufacturer argument is supplied
    if(is.null(manufacturer)){
      stop("New equipment model. Please supply manufacturer name.")
    }
    # make sure manufacturer is in org table
    sql2 <- RSQLite::dbSendStatement(db,
                                     "INSERT OR IGNORE into organizations
                                     (organizationtypecv, organizationcode, organizationname)
                                     VALUES
                                     ('Manufacturer', :organizationcode, :organizationname)")
    RSQLite::dbBind(sql2, param = list(organizationname = manufacturer,
                                       organizationcode = manufacturer))
    RSQLite::dbClearResult(res = sql2)
    # then add model to equipment models table
    sql3 <- RSQLite::dbSendStatement(db,
                                     "INSERT INTO equipmentmodels
                                     (modelmanufacturerid, modelname, isinstrument)
                                     VALUES (
                                     (SELECT organizationid FROM organizations
                                     WHERE organizationname = :organizationname),
                                     :modelname, 'TRUE'
                                     )")
    RSQLite::dbBind(sql3, param = list(organizationname = manufacturer,
                                       modelname = model_name))
    RSQLite::dbClearResult(res = sql3)
  }
  # make sure vendor is in organizations table
  sql4 <- RSQLite::dbSendStatement(db,
                                   "INSERT OR IGNORE into organizations
                                   (organizationtypecv, organizationcode, organizationname)
                                   VALUES
                                   ('Vendor', :organizationcode, :organizationname)")
  RSQLite::dbBind(sql4, param = list(organizationname = vendor,
                                     organizationcode = vendor))
  RSQLite::dbClearResult(res = sql4)
  # make sure owner is in people table
  sql5 <- RSQLite::dbSendStatement(db,
                                   "SELECT personid FROM people WHERE personfirstname = :personfirstname")
  RSQLite::dbBind(sql5, param = list(personfirstname = owner_first))
  ownerid <- DBI::dbFetch(sql5)
  RSQLite::dbClearResult(sql5)
  if(nrow(ownerid) < 1){
    # if not, check that last name and email argument is supplied
    if(is.null(owner_last) | is.null(owner_email)){
      stop("Owner is new person. Please supply owner_last and owner_email.")
    }
    rodm2::db_describe_person(db,PersonFirstName = owner_first,
                              PersonLastName = owner_last, PrimaryEmail = owner_email)
  }
  # update equipment table
  sql6 <- RSQLite::dbSendStatement(db,
                                   "INSERT into equipment
                                   (equipmentcode, equipmentname, equipmenttypecv,
                                   equipmentmodelid, equipmentserialnumber,
                                   equipmentownerid, equipmentvendorid,
                                   equipmentpurchasedate)
                                   VALUES
                                   (:equipmentcode, :equipmentname, :equipmenttype,
                                   (SELECT equipmentmodelid FROM equipmentmodels WHERE modelname = :modelname),
                                   :equipmentserialnumber,
                                   (SELECT personid FROM people WHERE personfirstname = :personfirstname),
                                   (SELECT organizationid FROM organizations WHERE organizationname = :vendorname),
                                   :equipmentpurchasedate)")
  RSQLite::dbBind(sql6, params = list(equipmentcode = equip_name,
                                      equipmentname = equip_name,
                                      equipmenttype = equipment_type,
                                      modelname = model_name,
                                      equipmentserialnumber = serial_no,
                                      personfirstname = owner_first,
                                      vendorname = vendor,
                                      equipmentpurchasedate = purchase_date))
  RSQLite::dbClearResult(sql6)
  message(paste0(owner_first,"'s ", model_name, " ",
                 equipment_type, " ", equip_name,
                 " has been added to the Equipment table."))
  }
  if (class(db) == "PostgreSQLConnection"){
    sql1 <- DBI::sqlInterpolate(db,
                                'SELECT equipmentmodelid FROM odm2.equipmentmodels WHERE modelname = ?modelname',
                                modelname = model_name)
    modelid <- RPostgreSQL::dbGetQuery(db, sql1)
    # if model is new, check that manufacturer is supplied
    if(nrow(modelid) <1){
      if(is.null(manufacturer)){
        stop("New equipment model. Please supply manufacturer name.")
      }
      # add manufacturer to organization table if not there already
      sql2 <- DBI::sqlInterpolate(db,
                                  "INSERT into odm2.organizations
                                  (organizationtypecv, organizationcode, organizationname)
                                  SELECT
                                  'Manufacturer', ?organizationcode, ?organizationname
                                  WHERE NOT EXISTS (SELECT organizationid FROM odm2.organizations
                                  WHERE organizationname = ?manufacturer);",
                                  organizationcode = manufacturer,
                                  organizationname = manufacturer,
                                  manufacturer = manufacturer)
      RPostgreSQL::dbGetQuery(db, sql2)
      # then add model to equipment models table
      sql3 <- DBI::sqlInterpolate(db,
                                  "INSERT into odm2.equipmentmodels
                                  (modelmanufacturerid, modelname, isinstrument)
                                  VALUES (
                                  (SELECT organizationid FROM odm2.organizations
                                  WHERE organizationname = ?manufacturer),
                                  ?modelname, 'TRUE')",
                                  manufacturer = manufacturer,
                                  modelname = model_name)
      RPostgreSQL::dbGetQuery(db, sql3)
    }
      # add vendor to organization table if not there already
      sql4 <- DBI::sqlInterpolate(db,
                                  "INSERT into odm2.organizations
                                  (organizationtypecv, organizationcode, organizationname)
                                  SELECT
                                  'Vendor', ?organizationcode, ?organizationname
                                  WHERE NOT EXISTS (SELECT organizationid FROM odm2.organizations
                                  WHERE organizationname = ?vendor);",
                                  organizationcode = vendor,
                                  organizationname = vendor,
                                  vendor = vendor)
      RPostgreSQL::dbGetQuery(db, sql4)
      # make sure owner is in people table
      sql5 <- DBI::sqlInterpolate(db,
                                  "SELECT personid FROM odm2.people WHERE personfirstname = ?personfirstname",
                                  personfirstname = owner_first)
      ownerid <- RPostgreSQL::dbGetQuery(db, sql5)
      if(nrow(ownerid) < 1){
        if(is.null(owner_last) | is.null(owner_email)){
          stop("Owner is new person. Please supply last name and email.")
        }
        rodm2::db_describe_person(db, PersonFirstName = owner_first,
                                  PersonLastName = owner_last, PrimaryEmail = owner_email)
      }
      # update equipment table
      sql6 <- DBI::sqlInterpolate(db,
                                  "INSERT into odm2.equipment
                                  (equipmentcode, equipmentname, equipmenttypecv,
                                  equipmentmodelid, equipmentserialnumber,
                                  equipmentownerid, equipmentvendorid,
                                  equipmentpurchasedate)
                                  VALUES
                                  (?equipmentcode, ?equipmentname, ?equipmenttype,
                                  (SELECT equipmentmodelid FROM odm2.equipmentmodels WHERE modelname = ?modelname),
                                  ?equipmentserialnumber,
                                  (SELECT personid FROM odm2.people WHERE personfirstname = ?personfirstname),
                                  (SELECT organizationid FROM odm2.organizations WHERE organizationname = ?vendorname),
                                  ?equipmentpurchasedate)",
                                  equipmentcode = equip_name,
                                  equipmentname = equip_name,
                                  equipmenttype = equipment_type,
                                  modelname = model_name,
                                  equipmentserialnumber = serial_no,
                                  personfirstname = owner_first,
                                  vendorname = vendor,
                                  equipmentpurchasedate = as.character(purchase_date))
      RPostgreSQL::dbGetQuery(db, sql6)
      message(paste0(owner_first,"'s ", model_name, " ",
                     equipment_type, " ", equip_name,
                     " has been added to the Equipment table."))
  }
}

#' Get list of equipment currently in database
#'
#' @param db database connection object
#'
#' @return the current values in the equipmentcode column of the equipment table
#' @export
#'
#' @examples
#' #db_get_methods(db)
db_get_equipment <- function(db){
  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}
  current_equipment <- c()
  if (class(db) == "SQLiteConnection"){
    current_equipment <- DBI::dbGetQuery(db, "SELECT equipmentname FROM equipment")[[1]]
  }
  if (class(db) == "PostgreSQLConnection"){
    current_equipment <- DBI::dbGetQuery(db, "SELECT equipmentname FROM odm2.equipment")[[1]]
  }
  return(current_equipment)
}

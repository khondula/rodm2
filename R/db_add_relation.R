#' Add relationships to related features table
#'
#' @param db database connection object
#' @param edgelist dataframe with new relationships to add. see details
#' @param from_column name of first column with feature codes to relate
#' @param to_column name of second column with feature codes to relate
#' @param relationtype_column name of column with relationship type from [controlled vocabulary](http://vocabulary.odm2.org/relationshiptype/)
#' @param relationtype relationship type to use if no relationship type column. defaults to "is child of"
#'
#' @return message with how many rows added
#' @export
#'
#' @examples
#' # df <- data.frame(
#' # "SamplingFeatureCode" = "subsite1",
#' # "RelatedFeatureCode" = "site1")
#' # db_add_relation(db, edgelist = df)
db_add_relation <- function(db,
                            edgelist,
                            from_column = "SamplingFeatureCode",
                            to_column = "RelatedFeatureCode",
                            relationtype_column = NULL,
                            relationtype = "isChildOf"){

  if (!class(db) %in% c("SQLiteConnection")) {
    stop("sorry, only sqlite connections are supported so far")}

  if(any(!is.data.frame(edgelist) & ncol(edgelist) < 2)){
    stop("edgelist is not data frame with at least 2 columns")
  }

  df <- edgelist
  db_sites <- rodm2::db_get_sites(db)
  db_sf <- RSQLite::dbReadTable(db, "samplingfeatures")
  db_sf <- db_sf[,c("SamplingFeatureID", "SamplingFeatureCode")]

  # check that all site codes are in the database
  if(any(!df[[from_column]] %in% db_sites) | any(!df[[to_column]] %in% db_sites)){
    from_missing <- df[[from_column]][!df[[from_column]] %in% db_sites]
    to_missing <- df[[to_column]][!df[[to_column]] %in% db_sites]

    sites_missing <- union(from_missing, to_missing)
    stop(paste("following sites are missing. add with db_describe_site.",
               list(sites_missing)))
  }

  if(is.null(relationtype_column)){
    df$RelationshipTypeCV <- as.character(relationtype)
  }

  df_to_add <- df %>%
    dplyr::rename(SamplingFeatureCode = from_column) %>%
    dplyr::rename(RelatedFeatureCode = to_column) %>%
    dplyr::left_join(db_sf, by = c("RelatedFeatureCode" = "SamplingFeatureCode")) %>%
    dplyr::rename(RelatedFeatureID = SamplingFeatureID) %>%
    dplyr::left_join(db_sf, by = c("SamplingFeatureCode")) %>%
    dplyr::select(SamplingFeatureID, relationtype_column, RelatedFeatureID) %>%
    dplyr::rename(RelationshipTypeCV = relationtype_column)

  RSQLite::dbWriteTable(db,
                        "relatedfeatures",
                        value = df_to_add,
                        append = TRUE)

  message(paste(nrow(df_to_add), "new relations added"))

}

# hs_sources <- read_csv("../Delmarva/data/hs-sources/hs_sources_all.csv")
# new_related_samples = hs_sources
# lhc = "SourceAirID"
# relationshiptypecv = "Is part of"
# rhc = "SampleID"

#' Insert new sample relationships to the related features table
#'
#' @param new_related_samples data frame with one row for each new relationship
#' @param lhc column with sample codes
#' @param relationshiptypecv relationship type from controlled vocabulary
#' @param rhc column with sample codes of related features
#'
#' @return true if successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_insert_sample_relations(new_related_samples, "hs_sample", "source_air", "Is part of")
#' }
db_insert_sample_relations <- function(new_related_samples, lhc, relationshiptypecv, rhc){

  # get specimens from database
  specimens <- dbGetQuery(db, "SELECT samplingfeatureid, samplingfeaturecode FROM odm2.samplingfeatures WHERE samplingfeaturetypecv = 'Specimen'")
  names(new_related_samples)[which(names(new_related_samples)==rhc)] <- "related_sample"
  names(new_related_samples)[which(names(new_related_samples)==lhc)] <- "sample"

  new_related_samples <- new_related_samples %>%
    left_join(specimens, by = c("related_sample" = "samplingfeaturecode")) %>%
    rename(relatedfeatureid = samplingfeatureid) %>%
    mutate(relationshiptypecv = relationshiptypecv) %>%
    left_join(specimens, by = c("sample" = "samplingfeaturecode")) %>%
    dplyr::select(samplingfeatureid, relationshiptypecv, relatedfeatureid)

  dbWriteTable(db, c("odm2","relatedfeatures"),
               new_related_samples,
               overwrite = FALSE,
               row.names = FALSE,
               append = TRUE)
}

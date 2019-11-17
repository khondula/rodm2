
#' Shiny gadget to get site codes of interest
#'
#' @details
#' Use site group annotations and site types to
#' identify site codes to use in db_get_results()
#'
#' @param db database connection object
#'
#' @return selected site codes
#' @export
#' @family interactive helpers
#' @examples
#' # my_sites <- get_site_codes(db)
#'
get_site_codes <- function(db){

  # get all site names
  current_sites <- DBI::dbGetQuery(db, "SELECT samplingfeaturetypecv, samplingfeaturecode, samplingfeaturename FROM samplingfeatures
                                     WHERE samplingfeaturetypecv != 'Specimen'")

  # site types
  site_types_used <- unique(current_sites[["SamplingFeatureTypeCV"]])
  current_site_codes <- unique(current_sites[["SamplingFeatureCode"]])

  n_sitetypes <- length(site_types_used)

  # site groups
  sf_annotation_types <- c("Site group", "Sampling feature annotation",
                           "Site annotation", "Specimen annotation",
                           "Specimen group")

  current_site_annotations <- DBI::dbGetQuery(db,
                    "SELECT sf.samplingfeaturecode, ann.annotationtypecv, ann.annotationtext from annotations ann
                    left join samplingfeatureannotations sfa ON sfa.annotationid = ann.annotationid
                    left join samplingfeatures sf ON sf.samplingfeatureid = sfa.samplingfeatureid")

  annotationtext_used <- unique(current_site_annotations[["AnnotationText"]])
  n_annotations <- length(annotationtext_used)

  # network
  current_site_network <-
    DBI::dbGetQuery(db,
                    "SELECT sf2.samplingfeaturecode as CHILD, sf.samplingfeaturecode as PARENT
                    FROM relatedfeatures rf
                    left join samplingfeatures sf ON sf.samplingfeatureid = rf.relatedfeatureid
                    left join samplingfeatures sf2 ON sf2.samplingfeatureid = rf.samplingfeatureid
                    WHERE relationshiptypecv = 'isChildOf'")

  site_network <- igraph::graph_from_data_frame(d = current_site_network, directed = TRUE)

  parent_nodes <- unique(current_site_network[["PARENT"]])

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Identify site codes that meet any of the following conditions:"),
    miniUI::miniContentPanel(
      # Define layout, inputs, outputs
      # shiny::p("Selected site codes:"),
      shiny::fillPage(
        # shiny::textOutput('selected_vars'),
        shiny::uiOutput('selectorsUI_types'),
        shiny::uiOutput('selectorsUI_groups'),
        shiny::selectizeInput(inputId = "selected_parents",
                              label = "Select subsites from:",
                              choices = parent_nodes,
                              multiple = TRUE),
        shiny::uiOutput('selectorsUI_network'))
    ))

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.
    # drop down for variable selection for each column

    output$selectorsUI_network <- shiny::renderUI({
      child_nodes <- igraph::adjacent_vertices(site_network,
                      v = input[['selected_parents']], mode = c( "in"))
      child_codes <- purrr::map(child_nodes, ~names(.x)) %>% unlist()
      child_codes <- unique(child_codes)
      shiny::selectizeInput(inputId = "selected_children",
                            label = "Select subsites:",
                            choices = child_codes,
                            multiple = TRUE)
    })

    output$selectorsUI_types <- shiny::renderUI({

      lapply(1:n_sitetypes, function(i){
        shiny::selectizeInput(inputId = sprintf("var%scode",i),
                              sprintf('Site type: %s', site_types_used[i]),
            choices = dplyr::filter(current_sites,
                      SamplingFeatureTypeCV == site_types_used[i])[["SamplingFeatureCode"]],
            multiple = TRUE)
      })

    })

    output$selectorsUI_groups <- shiny::renderUI({

      lapply(1:n_annotations, function(i){
        shiny::selectizeInput(inputId = sprintf("var%sgroup",i),
                              label = sprintf('Site group: %s', annotationtext_used[i]),
                              choices = dplyr::filter(current_site_annotations,
                                                      AnnotationText == annotationtext_used[i])[["SamplingFeatureCode"]],
                              multiple = TRUE)
      })

    })

    output$selected_vars <- shiny::renderText({
      unlist(lapply(1:n_sitetypes, function(i){
        input[[sprintf("var%scode", i)]]
      }))
    }, sep = ", ")

    # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {
      returnValue <- unique(c(lapply(1:n_sitetypes, function(i){
        input[[sprintf("var%scode", i)]]}),
        lapply(1:n_annotations, function(i){
        input[[sprintf("var%sgroup", i)]]
      }),input[['selected_children']]
      ))
      # names(returnValue) <- resulttypes_used
      returnValue <- unlist(returnValue)
      shiny::stopApp(returnValue)

    })
  }

  shiny::runGadget(ui, server)

}

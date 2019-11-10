
#' Shiny gadget to get variable codes of interest
#'
#' @param db database connection object
#'
#' @return selected variable codes
#' @export
#' @family interactive helpers

#' @examples
#' # my_vars <- get_variable_codes(db)
#'
get_variable_codes <- function(db){

  # figure out which types of variables are in results
  vars_used <- RSQLite::dbGetQuery(db, "SELECT DISTINCT variablecode, variablenamecv, resulttypecv
                                   from results
                                   left join variables var ON var.variableid = results.variableid
                                   ORDER BY resulttypecv ")

  vars_used
  resulttypes_used <- unique(vars_used[["ResultTypeCV"]])
  n_resulttypes <- length(resulttypes_used)


  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Identify variable codes with results"),
    miniUI::miniContentPanel(
      # Define layout, inputs, outputs
      shiny::fillRow(
        shiny::p("Selected variable codes:"),
        shiny::textOutput('selected_vars'),
        shiny::uiOutput('selectorsUIvar', inline = FALSE),
        flex = c(1,2,4)
      )
    ))

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.
    # drop down for variable selection for each column

    output$selectorsUIvar <- shiny::renderUI({

      lapply(1:n_resulttypes, function(i){
        shiny::checkboxGroupInput(inputId = sprintf("var%scode", i),
                                  label = sprintf('Variables with %s results', resulttypes_used[i]),
                                  choiceValues = dplyr::filter(vars_used, ResultTypeCV == resulttypes_used[i])[["VariableCode"]],
                                  selected = NULL,
                                  choiceNames = dplyr::filter(vars_used, ResultTypeCV == resulttypes_used[i])[["VariableNameCV"]]
        )
      })

    })

    output$selected_vars <- shiny::renderText({
      unlist(lapply(1:n_resulttypes, function(i){
        input[[sprintf("var%scode", i)]]
      }))
    }, sep = ", ")
    # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {
      returnValue <- lapply(1:n_resulttypes, function(i){
        input[[sprintf("var%scode", i)]]
      })
      # names(returnValue) <- resulttypes_used
      returnValue <- unlist(returnValue)
      shiny::stopApp(returnValue)

    })
  }

  shiny::runGadget(ui, server)

}

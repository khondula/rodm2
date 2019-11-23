

#' Make variables list
#'
#' @param data data frame with columns of data to describe
#' @param data_colnames
#' a string vector of column names with data values.
#' by default all columns except "Timestamp", "Sample", and "Site" if they are included.
#'
#' @return nested list for each column in data describing variable name and units
#' @export
#' @details This function creates the object needed for the variables argument
#' in db_insert_results_m, db_insert_results_ts, db_insert_results_samples,
#' db_insert_results_samples_profile, based on a data frame with the data to insert.
#' The data frame supplied should include only the columns with data and optionally a
#' column called Timestamp. For each column of data, provide a unique codename for the
#' variable as well as the \href{http://vocabulary.odm2.org/variablename}{term}
#' and \href{http://vocabulary.odm2.org/units}{units} from the controlled vocabularies. By
#' default the column name will be used as the variable code.
#' @family interactive helpers
#' @importFrom shiny renderUI
#' @importFrom glue glue_collapse
#' @importFrom glue glue
#' @importFrom glue glue_data
#' @importFrom dplyr bind_rows
#'
#'
#' @examples
#' \dontrun{
#' ts <- data.frame(
#'  Timestamp = c(
#'  "2018-06-27 13:45:00",
#'  "2018-06-27 13:55:00"),
#'   "wl" = c(1, 1.5))
#' vars_list <- make_vars_list(ts)
#' }
#'
make_vars_list <- function(data, data_colnames = setdiff(names(data), c("Timestamp", "Sample", "Site"))) {

  # data_colnames <- setdiff(names(data), c("Timestamp", "Sample", "Site"))
  suppressMessages(variablenamescv <- rodm2::get_cv_terms("variablename", quietly = TRUE))
  suppressMessages(unitscv <- rodm2::get_cv_terms("units", quietly = TRUE))

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Specify code and controlled vocab (CV) for each column"),
    miniUI::miniContentPanel(
      # Define layout, inputs, outputs
      shiny::fillRow(
        shiny::uiOutput('selectorsUIcode'),
        shiny::uiOutput('selectorsUIvar'),
        shiny::uiOutput('selectorsUIunits'))
      ),  shiny::p("Code to recreate this output:"),
    shiny::verbatimTextOutput('vars_list_code_text')

    )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.
    # drop down for variable selection for each column
    output$selectorsUIcode <- shiny::renderUI({

      lapply(1:length(data_colnames), function(i){
        shiny::textInput(inputId = sprintf("var%scode", i),
                              label = sprintf('Variable code for \"%s\"', data_colnames[i]),
                              value = data_colnames[i])
      })

    })

    output$selectorsUIvar <- shiny::renderUI({

      lapply(1:length(data_colnames), function(i){
        shiny::selectizeInput(inputId = sprintf("var%sname", i),
                       label = sprintf('CV term for \"%s\"', data_colnames[i]),
                       choices = variablenamescv,
                       options = list(placeholder = 'Start typing here',
                                      onInitialize = I('function() { this.setValue(""); }')))
      })

    })
    output$selectorsUIunits <- renderUI({
      # drop down for units selection for each column
      lapply(1:length(data_colnames), function(i){
        shiny::selectizeInput(inputId = sprintf("var%sunits", i),
                       label = sprintf('CV units for \"%s\"', data_colnames[i]),
                       choices = unitscv,
                       options = list(placeholder = 'Start typing here',
                                      onInitialize = I('function() { this.setValue(""); }')))
      })

    })

    vars_list_code <- shiny::reactive({

      # if(exists(input[[sprintf('var%sname', data_colnames[1])]])){

      vals <- lapply(1:length(data_colnames), function(i){
        list('column' = data_colnames[i],
             'name' = input[[sprintf('var%sname', i)]],
             'units' = input[[sprintf('var%sunits', i)]])
      })

      vars_list_df <- vals %>%
        lapply(as.data.frame, stringsAsFactors = FALSE) %>%
        dplyr::bind_rows()

      glue_out <- glue::glue_data(vars_list_df,
                                  "'{column}' = list(column = '{column}', name = '{name}', units = '{units}')")
      glue_out <- glue::glue_collapse(glue_out, sep = ",\n")

      glue::glue("vars_list <- list({glue_out})")
    })

    output$vars_list_code_text <- shiny::renderText({
        vars_list_code()
    })

    # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {
      returnValue <- lapply(1:length(data_colnames), function(i){
        list('column' = data_colnames[i],
             'name' = input[[sprintf('var%sname', i)]],
             'units' = input[[sprintf('var%sunits', i)]])
      })
      names(returnValue) <- sapply(1:length(data_colnames), function(i){
        input[[sprintf('var%scode', i)]]
      })
      shiny::stopApp(returnValue)

    })
  }

  shiny::runGadget(ui, server)
}


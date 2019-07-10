

#' Make variables list
#'
#' @param data data frame wtih Timestamp column and other data
#'
#' @return nested list for each column in data describing variable name and units
#' @export
#'
#' @examples
#' \dontrun{
#' vars_list <- make_vars_list(ts_data)
#' }
#'
make_vars_list <- function(data) {

  data_colnames <- setdiff(names(data), "Timestamp")
  variablenamescv <- rodm2::get_cv_terms("variablename")
  unitscv <- rodm2::get_cv_terms("units")

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Please describe the data in each column"),
    miniUI::miniContentPanel(
      # Define layout, inputs, outputs
      shiny::fillRow(
        shiny::uiOutput('selectorsUIvar'),
        shiny::uiOutput('selectorsUIunits')
      )))

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.
    # drop down for variable selection for each column
    output$selectorsUIvar <- shiny::renderUI({

      lapply(1:length(data_colnames), function(i){
        shiny::selectizeInput(inputId = sprintf("var%sname", i),
                       label = sprintf('Column %s variable name', data_colnames[i]),
                       choices = variablenamescv,
                       options = list(placeholder = 'Start typing here',
                                      onInitialize = I('function() { this.setValue(""); }')))
      })

    })
    output$selectorsUIunits <- renderUI({
      # drop down for units selection for each column
      lapply(1:length(data_colnames), function(i){
        shiny::selectizeInput(inputId = sprintf("var%sunits", i),
                       label = sprintf('Column %s units', data_colnames[i]),
                       choices = unitscv,
                       options = list(placeholder = 'Start typing here',
                                      onInitialize = I('function() { this.setValue(""); }')))
      })

    })
    # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {
      returnValue <- lapply(1:length(data_colnames), function(i){
        list('column' = data_colnames[i], 'units' = input[[sprintf('var%sunits', i)]])
      })
      names(returnValue) <- sapply(1:length(data_colnames), function(i){
        input[[sprintf('var%sname', i)]]
      })
      shiny::stopApp(returnValue)

    })
  }

  shiny::runGadget(ui, server)
}




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
  suppressMessages(variablenamescv <- rodm2::get_cv_terms("variablename", quietly = TRUE))
  suppressMessages(unitscv <- rodm2::get_cv_terms("units", quietly = TRUE))

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Specify code and controlled vocab (CV) for each column"),
    miniUI::miniContentPanel(
      # Define layout, inputs, outputs
      shiny::fillRow(
        shiny::uiOutput('selectorsUIcode'),
        shiny::uiOutput('selectorsUIvar'),
        shiny::uiOutput('selectorsUIunits')
      )))

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


library(shiny)
library(miniUI)
library(ggplot2)

make_vars_list <- function(data){
  
  data_colnames <- setdiff(names(data), "Timestamp")
  variablenamescv <- rodm2::get_cv_terms("variablename")
  unitscv <- rodm2::get_cv_terms("units")
  
  ui <- miniPage(
    gadgetTitleBar("Create variable list agrument"),
    miniContentPanel(
      selectizeInput("varname", "Variable:", choices = variablenamescv),
      selectizeInput("var1col", "is in column:", choices = data_colnames),
      selectizeInput("var1units", "in units:", choices = unitscv),
      verbatimTextOutput("vars_list")
    )
  )
  
  server <- function(input, output, session) {
    # 
    new_vars_list <- reactiveValues(lst = vector("list", length(data_colnames)))

    output$vars_list <- renderText({
      paste0("list('", input$varname, "' = list(column = '", input$var1col, "' , units = '", input$var1units, "')")
    })
    
    observe({
      new_vars_list$lst <- input$varname
     })
    
    observe({
      new_vars_list$lst[["column"]] <- input$var1col
    })
    
    observe({
      new_vars_list$lst[["units"]] <- input$var1units
    })
    
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # stopApp(output$vars_list)
      stopApp(new_vars_list$lst)
    })
  }
  
  runGadget(ui, server)
}

varlistgadget <- make_vars_list(qbwind)

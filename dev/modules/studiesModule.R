# modules/studiesModule.R
studiesModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("studiesTable"))
  )
}

studiesModule <- function(input, output, session, studies_data) {
  output$studiesTable <- renderDT({
    req(studies_data())  # Ensure data exists
    datatable(studies_data())
  })
  return(reactive(input$studiesTable_rows_selected))
}

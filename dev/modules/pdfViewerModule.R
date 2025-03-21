# modules/pdfViewerModule.R
pdfViewerModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "PDF Viewer",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      uiOutput(ns("pdfViewer"))
    )
  )
}

pdfViewerModule <- function(input, output, session, selected_studies) {
  output$pdfViewer <- renderUI({
    req(selected_studies())
    tags$iframe(src = paste0("https://example.com/", selected_studies()[1]), width = "100%", height = "600px")
  })
}

# modules/downloadModule.R
downloadModuleUI <- function(id) {
  ns <- NS(id)
  downloadButton(ns("downloadBtn"), "Download Selected")
}

downloadModule <- function(input, output, session, selected_studies, bookmarked_pdfs) {
  output$downloadBtn <- downloadHandler(
    filename = function() { "selected_pdfs.zip" },
    content = function(file) { zip::zip(file, selected_studies()) }
  )
}

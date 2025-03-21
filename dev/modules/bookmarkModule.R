# modules/bookmarkModule.R
bookmarkActionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("bookmarkBtn"), "Bookmark Selected PDFs", icon = icon("bookmark")),
    actionButton(ns("deselectBtn"), "Deselect Selected PDFs", icon = icon("times"))
  )
}

bookmarkModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("bookmarkedTable")),
    fluidRow(
      box(
        title = "PDF Viewer",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        uiOutput(ns("pdfViewer"))
      )
    )
  )
}

bookmarkModule <- function(input, output, session, studies_data, selected_rows) {
  bookmarked <- reactiveVal(data.frame(
    filename = character(),
    docpath = character(),
    nct_id = character(),
    stringsAsFactors = FALSE
  ))

  observeEvent(input$bookmarkBtn, {
    data <- studies_data()
    rows <- selected_rows()
    if (is.null(data) || length(rows) == 0) return()
    selected <- data[rows, ]
    current <- bookmarked()
    new_bookmarks <- data.frame(
      filename = selected$filename,
      docpath = selected$docpath,
      nct_id = selected$protocolSection.identificationModule.nctId,
      stringsAsFactors = FALSE
    )
    unique_new <- new_bookmarks[!mapply(function(fn, id) {
      any(current$filename == fn & current$nct_id == id)
    }, new_bookmarks$filename, new_bookmarks$nct_id), ]
    if (nrow(unique_new) > 0) {
      bookmarked(bind_rows(current, unique_new))
      showNotification(sprintf("%d PDFs bookmarked.", nrow(unique_new)))
    } else {
      showNotification("No new PDFs to bookmark.", type = "warning")
    }
  })

  observeEvent(input$deselectBtn, {
    data <- studies_data()
    rows <- selected_rows()
    if (is.null(data) || length(rows) == 0) return()
    selected <- data[rows, ]
    current <- bookmarked()
    updated <- current %>%
      filter(!mapply(function(fn, id) {
        any(selected$filename == fn & selected$protocolSection.identificationModule.nctId == id)
      }, filename, nct_id))
    bookmarked(updated)
    showNotification("Selected PDFs removed from bookmarks.")
  })

  output$bookmarkedTable <- renderDT({
    datatable(bookmarked(), selection = "single", options = list(pageLength = 10, searchHighlight = TRUE))
  })

  output$pdfViewer <- renderUI({
    req(input$bookmarkedTable_rows_selected)
    selected <- bookmarked()[input$bookmarkedTable_rows_selected, , drop = FALSE]
    req(nrow(selected) == 1, !is.na(selected$docpath))
    tags$iframe(src = selected$docpath, width = "100%", height = "800px")
  })

  return(bookmarked)
}

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
    ),
    fluidRow(
      box(
        title = "Download",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        downloadLink(ns("downloadBookmarked"), "Download Bookmarked PDFs", class = "btn btn-primary")
      )
    )
  )
}

bookmarkActionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("bookmarkBtn"), "Bookmark Selected PDFs", icon = icon("bookmark"),
                 title = "Add selected studies to your bookmarks"),
    actionButton(ns("deselectBtn"), "Deselect Selected PDFs", icon = icon("times"),
                 title = "Remove selected studies from your bookmarks")
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
      runjs("$('#bookmarks-bookmarkedTable').addClass('highlighted')")
      delay(1000, runjs("$('#bookmarks-bookmarkedTable').removeClass('highlighted')"))
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

  output$downloadBookmarked <- downloadHandler(
    filename = function() {
      paste0("bookmarked_pdfs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      bookmarks <- bookmarked()

      if (is.null(bookmarks) || nrow(bookmarks) == 0) {
        showNotification("No bookmarks to download.", type = "error")
        return()
      }

      temp_dir <- tempfile("bookmark_download_")
      dir.create(temp_dir)

      downloaded_files_raw <- vapply(seq_along(bookmarks$docpath), function(i) {
        url <- bookmarks$docpath[i]
        name <- bookmarks$filename[i]
        safe_name <- gsub("[^A-Za-z0-9_.-]", "_", name)
        path <- file.path(temp_dir, safe_name)
        print(paste("Downloading:", url))
        tryCatch({
          download.file(url, path, mode = "wb", quiet = TRUE)
          if (file.exists(path) && file.size(path) > 0) path else NA_character_
        }, error = function(e) {
          print(paste("Download failed:", url))
          NA_character_
        })
      }, FUN.VALUE = character(1))

      total_attempted <- length(downloaded_files_raw)
      downloaded_files <- downloaded_files_raw[!is.na(downloaded_files_raw) & file.exists(downloaded_files_raw)]

      if (length(downloaded_files) == 0) {
        showNotification("No PDFs were downloaded successfully.", type = "error")
        return(NULL)
      }

      zipfile <- paste0(tempfile("pdfs_"), ".zip")
      zip::zip(zipfile, files = downloaded_files, mode = "cherry-pick")
      file.copy(zipfile, file, overwrite = TRUE)

      showNotification(sprintf(
        "%d out of %d PDFs downloaded successfully.",
        length(downloaded_files), total_attempted
      ), type = "message")
    },
    contentType = "application/zip"
  )

  return(bookmarked)
}

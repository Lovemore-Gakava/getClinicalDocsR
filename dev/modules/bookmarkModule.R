bookmarkModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "overflow-x:auto;",
        DTOutput(ns("bookmarkedTable"))
    ),
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
        checkboxInput(ns("includeCSV"), "Include CSV metadata file", value = TRUE, width = "100%"),
        checkboxInput(ns("includeReadme"), "Include README.txt file", value = TRUE, width = "100%"),
        verbatimTextOutput(ns("fileSizeInfo")),
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
    req(nrow(selected) == 1, !is.na(selected$docpath), nzchar(selected$docpath))
    tags$iframe(src = selected$docpath, width = "100%", height = "800px", frameborder = 0)
  })

  output$fileSizeInfo <- renderText({
    bookmarks <- bookmarked()
    if (is.null(bookmarks) || nrow(bookmarks) == 0) return("")
    temp_dir <- tempfile()
    dir.create(temp_dir)
    sizes <- sapply(bookmarks$docpath, function(url) {
      safe_name <- gsub("[^A-Za-z0-9_.-]", "_", url)
      temp <- tempfile(tmpdir = temp_dir)
      tryCatch({
        download.file(url, temp, mode = "wb", quiet = TRUE)
        file.info(temp)$size
      }, error = function(e) NA)
    })
    total <- sum(sizes, na.rm = TRUE) / 1024^2
    sprintf("Estimated total download size: %.2f MB", total)
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
        nct_id <- bookmarks$nct_id[i]
        safe_name <- gsub("[^A-Za-z0-9_.-]", "_", paste0(nct_id, "_", name))
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

      files_to_zip <- downloaded_files

      if (isTruthy(input$includeCSV)) {
        metadata_path <- file.path(temp_dir, "bookmarked_list.csv")
        write.csv(bookmarks, metadata_path, row.names = FALSE)
        files_to_zip <- c(files_to_zip, metadata_path)
      }

      if (isTruthy(input$includeReadme)) {
        readme_path <- file.path(temp_dir, "README.txt")
        writeLines(c(
          "ClinicalTrials.gov PDF Download",
          "",
          "This ZIP contains the bookmarked PDFs downloaded from ClinicalTrials.gov.",
          "Each PDF file is prefixed with its corresponding NCT ID.",
          "",
          "The CSV file (bookmarked_list.csv) includes metadata for each document.",
          "",
          paste("Generated on:", as.character(Sys.time()))
        ), con = readme_path)
        files_to_zip <- c(files_to_zip, readme_path)
      }

      zipfile <- paste0(tempfile("pdfs_"), ".zip")
      zip::zip(zipfile, files = files_to_zip, mode = "cherry-pick")
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

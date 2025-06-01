library(shiny)
library(dplyr)
library(DT)
# modules/studiesModule.R
studiesModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "margin-bottom: 10px;",
        textOutput(ns("studyCount"))
    ),
    div(style = "overflow-x:auto;",
        DTOutput(ns("studiesTable"))
    )
  )
}

studiesModule <- function(input, output, session, studies_data) {
  
  output$studyCount <- renderText({
    data <- studies_data()
    if(is.null(data) || nrow(data) == 0) {
      "No studies loaded. Use the search panel to find studies."
    } else {
      docs_count <- sum(!is.na(data$docpath), na.rm = TRUE)
      sprintf("Showing %d studies (%d with downloadable documents)", 
              nrow(data), docs_count)
    }
  })
  output$studiesTable <- renderDT({
    req(studies_data())
    data <- studies_data()
    
    # Find actual field names dynamically
    nct_field <- names(data)[grepl('nct.*id', names(data), ignore.case = TRUE)][1]
    title_field <- names(data)[grepl('brief.*title|title', names(data), ignore.case = TRUE)][1]
    status_field <- names(data)[grepl('overall.*status|status', names(data), ignore.case = TRUE)][1]
    phase_field <- names(data)[grepl('phase', names(data), ignore.case = TRUE)][1]
    
    # Fallback to expected field names if not found
    if(is.na(nct_field)) nct_field <- "protocolSection.identificationModule.nctId"
    if(is.na(title_field)) title_field <- "protocolSection.identificationModule.briefTitle"
    if(is.na(status_field)) status_field <- "protocolSection.statusModule.overallStatus"
    if(is.na(phase_field)) phase_field <- "protocolSection.designModule.phases"
    
    # Build display data with dynamic field selection
    display_data <- data
    
    # Rename columns that exist
    if(nct_field %in% names(data)) {
      display_data <- display_data |> rename("NCT ID" = all_of(nct_field))
    }
    if(title_field %in% names(data)) {
      display_data <- display_data |> rename("Title" = all_of(title_field))
    }
    if(status_field %in% names(data)) {
      display_data <- display_data |> rename("Status" = all_of(status_field))
    }
    if(phase_field %in% names(data)) {
      display_data <- display_data |> rename("Phase" = all_of(phase_field))
    }
    
    # Select columns that actually exist and clean up
    available_cols <- intersect(c("NCT ID", "Title", "Status", "Phase", "Start Date", "Completion Date", "filename", "typeAbbrev", "docpath"), names(display_data)) # Added "Completion Date"
    
    if(length(available_cols) > 0) {
      display_data <- display_data |>
        select(all_of(available_cols), everything()) |>
        # Clean up the display
        mutate(
          across(c("Phase"), ~ ifelse(is.na(.x) | .x == "", "Not Applicable", .x)),
          across(c("Start Date", "Completion Date"), ~ as.character(.x)), # Added "Completion Date"
          across(c("filename"), ~ ifelse(is.na(.x), "No documents", .x))
        )
      
      # Rename document columns if they exist
      if("filename" %in% names(display_data)) {
        display_data <- display_data |> rename("Document" = filename)
      }
      if("typeAbbrev" %in% names(display_data)) {
        display_data <- display_data |> rename("Document Type" = typeAbbrev)
      }
    }
    
    # Only allow selection for rows with a downloadable document
    selectable <- !is.na(data$docpath) & nzchar(data$docpath)
    datatable(
      display_data,
      selection = list(mode = "multiple", target = "row", selectable = which(selectable)),
      options = list(
        pageLength = 100, # Match bookmarkedTable page length
        searchHighlight = TRUE,
        dom = 'Bfrtip', # Enable Buttons toolbar
        buttons = c('colvis') # Add column visibility button
      ),
      extensions = 'Buttons',
      rownames = FALSE
    )
  })
  
  observeEvent(input$studiesTable_rows_selected, {
    data <- studies_data()
    selected <- input$studiesTable_rows_selected
    if (length(selected) > 0) {
      # Check if any selected rows do not have a downloadable document
      invalid <- selected[is.na(data$docpath[selected]) | !nzchar(data$docpath[selected])]
      if (length(invalid) > 0) {
        showNotification("Only studies with downloadable documents can be selected.", type = "warning")
        # Deselect invalid rows
        proxy <- dataTableProxy("studiesTable")
        selectRows(proxy, selected[!selected %in% invalid])
      }
    }
  })
  
  return(reactive({
    input$studiesTable_rows_selected
  }))
}

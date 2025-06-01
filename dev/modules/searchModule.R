# modules/searchModule.R
searchModuleUI <- function(id) {
  ns <- NS(id)
  div(
    style = "padding: 20px; background-color: #f8f9fa; min-height: 100vh;",
    
    # Header
    div(
      style = "text-align: center; margin-bottom: 25px; border-bottom: 2px solid #dee2e6; padding-bottom: 15px;",
      h3("Clinical Trials Search", style = "color: #495057; margin-bottom: 5px;"),
      p("Find studies and documents", style = "color: #6c757d; margin: 0; font-size: 14px;")
    ),
    
    # Main search fields
    div(
      style = "margin-bottom: 20px;",
      h5("Primary Search", style = "color: #495057; margin-bottom: 15px;"),
      
      div(
        style = "margin-bottom: 15px;",
        textInput(ns("condition"), "Medical Condition:", 
                  value = "cancer", 
                  placeholder = "e.g., lung cancer, diabetes",
                  width = "100%")
      ),
      
      div(
        style = "margin-bottom: 15px;",
        textInput(ns("intervention"), "Treatment/Intervention:", 
                  value = "", 
                  placeholder = "e.g., drug name, therapy type",
                  width = "100%")
      )
    ),
    
    # Filters section
    div(
      style = "margin-bottom: 20px; padding: 15px; background-color: white; border-radius: 8px; border: 1px solid #e9ecef;",
      h5("Filters", style = "color: #495057; margin-bottom: 15px; margin-top: 0;"),
      
      div(
        style = "margin-bottom: 15px;",
        selectInput(ns("status"), "Study Status:",
                   choices = list("All Statuses" = "",
                                "Recruiting" = "Recruiting",
                                "Active (not recruiting)" = "Active, not recruiting",
                                "Completed" = "Completed",
                                "Suspended" = "Suspended",
                                "Terminated" = "Terminated",
                                "Withdrawn" = "Withdrawn"),
                   selected = "",
                   width = "100%")
      ),
      
      div(
        style = "margin-bottom: 15px;",
        selectInput(ns("phase"), "Study Phase:",
                   choices = list("All Phases" = "",
                                "Early Phase 1" = "Early Phase 1",
                                "Phase 1" = "Phase 1",
                                "Phase 2" = "Phase 2", 
                                "Phase 3" = "Phase 3",
                                "Phase 4" = "Phase 4",
                                "Not Applicable" = "Not Applicable"),
                   selected = "",
                   width = "100%")
      ),
        div(
        style = "margin-bottom: 15px;",        dateInput(ns("startDate"), "Updated After:", 
                  value = NULL,  # Use NULL instead of NA to avoid validation issues
                  format = "yyyy-mm-dd",
                  max = Sys.Date(),  # Prevent future dates
                  width = "100%")
      ),
      
      div(
        style = "margin-bottom: 10px;",
        numericInput(ns("pageSize"), "Maximum Results:", 
                    value = 1000, min = 1, max = 50000,
                    width = "100%")
      )
    ),
    
    # Options section
    div(
      style = "margin-bottom: 25px; padding: 15px; background-color: white; border-radius: 8px; border: 1px solid #e9ecef;",
      h5("Downloadable Studies", style = "color: #495057; margin-bottom: 15px; margin-top: 0;"),
      
      div(
        style = "margin-bottom: 0px; display: flex; align-items: center; height: 32px;",
        checkboxInput(ns("documentsOnly"), 
                     "Show only studies with downloadable documents", 
                     value = FALSE,
                     width = "auto"),
        style = "margin: 0; padding: 0;"
      )
    ),
      # Action buttons
    div(
      style = "text-align: center; margin-bottom: 20px;",
      actionButton(ns("searchBtn"), "Search Studies", 
                  icon = icon("search"), 
                  class = "btn-primary",
                  style = "width: 100%; margin-bottom: 10px; padding: 12px; font-size: 16px; font-weight: bold;"),
      
      actionButton(ns("clearBtn"), "Clear All Filters", 
                  icon = icon("eraser"),
                  class = "btn-outline-secondary",
                  style = "width: 100%; padding: 8px; margin-bottom: 10px;"),
      
      actionButton(ns("testBtn"), "Test API Connection", 
                  icon = icon("wifi"),
                  class = "btn-outline-info",
                  style = "width: 100%; padding: 6px; font-size: 12px;")
    ),
    
    # Status display
    div(
      id = ns("searchStatus"), 
      style = "margin-top: 15px;"
    )
  )
}

searchModule <- function(input, output, session) {
  studies_data <- reactiveVal(NULL)
  search_info <- reactiveVal(NULL)
    # Enhanced API fetch function with better retry logic and error handling
  fetch_page <- function(url, params, max_retries = 3) {
    for(attempt in 1:max_retries) {
      tryCatch({
        cat("Attempt", attempt, "- URL:", url, "\n")
        cat("Parameters:", paste(names(params), params, sep = "=", collapse = "&"), "\n")
        
        # Use more conservative timeout and user agent
        response <- GET(url, 
                       query = params, 
                       timeout(60),  # Increased timeout
                       add_headers(
                         "User-Agent" = "R/httr clinical-trials-viewer",
                         "Accept" = "application/json"
                       ))
        
        cat("Response status:", status_code(response), "\n")
        
        if (status_code(response) == 500) {
          stop(paste("Server error (500). The search parameters may be too complex or the API is temporarily unavailable."))
        } else if (http_error(response)) {
          stop(paste("API request failed with status code", status_code(response)))
        }
          content_text <- content(response, as = "text", encoding = "UTF-8")
        result <- fromJSON(content_text, flatten = TRUE)
        
        cat("Successfully parsed JSON response\n")
        cat("Result structure:", paste(names(result), collapse = ", "), "\n")
        if("studies" %in% names(result)) {
          cat("Found studies field, type:", class(result$studies), "length:", length(result$studies), "\n")
        } else {
          cat("No studies field in result\n")
        }
        
        return(result)
        
      }, error = function(e) {
        cat("Attempt", attempt, "failed:", e$message, "\n")
        if(attempt == max_retries) {
          stop(paste("Failed after", max_retries, "attempts:", e$message))
        }
        Sys.sleep(2^attempt) # Exponential backoff
      })
    }
  }
  # Build query parameters based on inputs
  build_query_params <- function() {
    params <- list(
      format = "json",
      pageSize = min(input$pageSize, 1000)
    )
    
    # Add condition using query.cond
    if(nzchar(input$condition)) {
      params[["query.cond"]] <- input$condition
    }
    
    # Add intervention using query.intr 
    if(nzchar(input$intervention)) {
      params[["query.intr"]] <- input$intervention
    }
    
    # Build query.term for additional filters (correct API format)
    query_terms <- c()
    
    # Add study status filter using proper syntax
    if(nzchar(input$status)) {
      query_terms <- c(query_terms, paste0("AREA[OverallStatus]", input$status))
    }
    
    # Add phase filter using proper syntax
    if(nzchar(input$phase)) {
      query_terms <- c(query_terms, paste0("AREA[Phase]", gsub(" ", "%20", input$phase)))
    }
    
    # Add date filter using proper syntax - only if user actually selected a date
    if(!is.null(input$startDate) && !is.na(input$startDate) && input$startDate != Sys.Date()) {
      # Make sure the date is not in the future (already validated above)
      query_terms <- c(query_terms, paste0("AREA[LastUpdatePostDate]RANGE[", input$startDate, ",MAX]"))
    }
    
    # Combine query terms if any exist
    if(length(query_terms) > 0) {
      params[["query.term"]] <- paste(query_terms, collapse = " AND ")
    }
      # Use correct field names based on API v2 structure
    # Don't specify fields to get full response, then we'll work with actual structure
    # params[["fields"]] <- paste(fields, collapse = ",")
    
    return(params)
  }
    # Clear button functionality
  observeEvent(input$clearBtn, {
    updateTextInput(session, "condition", value = "cancer")  # Reset to default
    updateTextInput(session, "intervention", value = "")
    updateSelectInput(session, "status", selected = "")
    updateSelectInput(session, "phase", selected = "")
    updateDateInput(session, "startDate", value = NULL)
    updateCheckboxInput(session, "documentsOnly", value = TRUE)
    updateNumericInput(session, "pageSize", value = 100)
    
    # Clear results
    studies_data(NULL)
    search_info(NULL)
      removeUI(selector = paste0("#", session$ns("searchStatus"), " > *"))
    
    showNotification("Filters cleared and reset to defaults", type = "message")
  })

  observeEvent(input$searchBtn, {
    # Validate inputs
    if(!nzchar(input$condition) && !nzchar(input$intervention)) {
      showNotification("Please enter at least a condition or intervention to search.", 
                      type = "warning")
      return()
    }
    # Validate date is not in future - handle NULL and NA values properly
    if(!is.null(input$startDate) && !is.na(input$startDate)) {
      if(input$startDate > Sys.Date()) {
        showNotification("Date cannot be in the future. Please select a past date.", 
                        type = "warning")
        return()
      }
    }
    
    # Clear previous status
    removeUI(selector = paste0("#", session$ns("searchStatus"), " > *"))
    
    withProgress(message = 'Searching ClinicalTrials.gov...', value = 0, {
      base_url <- "https://clinicaltrials.gov/api/v2/studies"
      query_params <- build_query_params()
      incProgress(0.2, detail = "Connecting to API...")
      tryCatch({
        start_time <- Sys.time()
        page_data <- fetch_page(base_url, query_params)
        incProgress(0.3, detail = "Processing study data...")        
        if("studies" %in% names(page_data)) {
          # Studies field exists, proceed with processing
        }
        # Check for studies and convert to dataframe properly
        if(!"studies" %in% names(page_data) || length(page_data$studies) == 0) {
          showNotification("No studies found matching your criteria. Try broader search terms.", type = "warning")
          studies_data(NULL)
          search_info(list(
            total_studies = 0,
            query = paste(names(query_params)[names(query_params) %in% c("query.cond", "query.intr")], 
                         query_params[names(query_params) %in% c("query.cond", "query.intr")], 
                         sep = "=", collapse = " & "),
            search_time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
          ))
          insertUI(
            selector = paste0("#", session$ns("searchStatus")),
            where = "beforeEnd",
            ui = div(class = "alert alert-warning",
                    icon("info-circle"), " No studies found. Try broader search terms.")
          )
          return()
        }
        studies <- as.data.frame(page_data$studies)
        incProgress(0.3, detail = "Processing documents...")
        studies_with_docs <- studies
        doc_fields <- names(studies)[grepl('document|doc', names(studies), ignore.case = TRUE)]
        cat("Document-related fields found:", paste(doc_fields, collapse = ", "), "\n")
        # Try to find and unnest documents if they exist
        if(length(doc_fields) > 0) {
          large_doc_field <- doc_fields[grepl('largeDocs|largeDocumentModule', doc_fields)]
          if(length(large_doc_field) > 0 && any(!is.na(studies[[large_doc_field[1]]]))) {
            tryCatch({
              studies_with_docs <- studies |>
                tidyr::unnest(cols = all_of(large_doc_field[1]), 
                             keep_empty = TRUE) # Always keep all studies
            }, error = function(e) {
              cat("Unnesting failed, using original data structure:", e$message, "\n")
              studies_with_docs <- studies
            })
          }
        } 
        
        incProgress(0.2, detail = "Building document URLs...")
          # Build document URLs more safely using actual field names
        root_largedocs <- "https://cdn.clinicaltrials.gov/large-docs"
        
        # Find the actual NCT ID field
        nct_field <- names(studies_with_docs)[grepl('nct.*id', names(studies_with_docs), ignore.case = TRUE)][1]
        if(is.na(nct_field)) nct_field <- "protocolSection.identificationModule.nctId"
        
        # Prepare NCT ID vector outside of mutate
        if (nct_field %in% names(studies_with_docs)) {
          nct_ids <- as.character(studies_with_docs[[nct_field]])
        } else {
          nct_ids <- rep(NA_character_, nrow(studies_with_docs))
        }
        studies_with_docs$nct_id <- nct_ids
        
        # Build studies_updated and assign docpath directly
        studies_updated <- studies_with_docs
        studies_updated$docpath <- ifelse(
          !is.na(studies_updated$filename) & nzchar(studies_updated$filename) & !is.na(studies_updated$nct_id),
          paste0(
            root_largedocs, "/", str_sub(studies_updated$nct_id, -2), "/",
            studies_updated$nct_id, "/", studies_updated$filename
          ),
          NA_character_
        )
        
        # Add readable date columns safely - vectorized outside mutate
        date_fields <- names(studies_updated)[grepl('date', names(studies_updated), ignore.case = TRUE)]
        if(length(date_fields) > 0) {
          start_date_field <- date_fields[grepl('start', date_fields, ignore.case = TRUE)][1]
          completion_date_field <- date_fields[grepl('completion|primary', date_fields, ignore.case = TRUE)][1]
          n <- nrow(studies_updated)
          # Vectorized start date
          if(start_date_field %in% names(studies_updated)) {
            studies_updated$"Start Date" <- tryCatch( # Renamed here
              as.Date(studies_updated[[start_date_field]]),
              error = function(e) rep(as.Date(NA), n)
            )
          } else {
            studies_updated$"Start Date" <- rep(as.Date(NA), n) # Renamed here
          }
          # Vectorized completion date
          if(completion_date_field %in% names(studies_updated)) {
            studies_updated$"Completion Date" <- tryCatch( # Renamed here
              as.Date(studies_updated[[completion_date_field]]),
              error = function(e) rep(as.Date(NA), n)
            )
          } else {
            studies_updated$"Completion Date" <- rep(as.Date(NA), n) # Renamed here
          }
        }
        
        end_time <- Sys.time()
        search_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
        studies_data(studies_updated)
        search_info(list(
          total_studies = nrow(studies_updated),
          total_available = page_data$totalCount %||% nrow(studies_updated),
          query = paste(names(query_params)[names(query_params) %in% c("query.cond", "query.intr")], 
                       query_params[names(query_params) %in% c("query.cond", "query.intr")], 
                       sep = "=", collapse = " & "),
          search_time = search_time
        ))
        
        # Update status display
        insertUI(
          selector = paste0("#", session$ns("searchStatus")),
          where = "beforeEnd",
          ui = div(
            class = "alert alert-success",
            icon("check-circle"), 
            sprintf(" Found %d studies%s in %.2f seconds", 
                   nrow(studies_updated),
                   if(input$documentsOnly) " with documents" else "",
                   search_time)
          )
        )
        
        showNotification(sprintf("Successfully loaded %d studies!", 
                               nrow(studies_updated)), type = "message") # Changed type to "message"
        
      }, error = function(e) {
        error_msg <- as.character(e$message)
        
        # Provide more specific error messages
        if(grepl("400", error_msg)) {
          user_msg <- "Bad request. Please check your search parameters - try simpler terms or remove date filters."
        } else if(grepl("500", error_msg)) {
          user_msg <- "Server error. This might be due to complex search terms. Try simpler search criteria."
        } else if(grepl("timeout|timed out", error_msg, ignore.case = TRUE)) {
          user_msg <- "Request timed out. Please try again or use fewer search terms."
        } else if(grepl("404", error_msg)) {
          user_msg <- "API endpoint not found. Please try again later."
        } else {
          user_msg <- paste("Search failed:", error_msg)
        }
        
        showNotification(user_msg, type = "error", duration = 8)
        
        insertUI(
          selector = paste0("#", session$ns("searchStatus")),
          where = "beforeEnd",
          ui = div(class = "alert alert-danger",
                  icon("exclamation-triangle"), " ", user_msg,
                  br(), br(),                  strong("Suggestions:"),
                  tags$ul(
                    tags$li("Try simpler search terms (e.g., just 'cancer')"),
                    tags$li("Remove date filters"),
                    tags$li("Remove status/phase filters"),
                    tags$li("Check for special characters in search terms"),
                    tags$li("Try the 'Test API Connection' button first")
                  ))
        )
      })
    })
  })
  
  # Test API connectivity with simple search
  test_api_connection <- function() {
    tryCatch({
      simple_params <- list(
        query.cond = "cancer",
        fields = "NCTId,BriefTitle",        format = "json",
        pageSize = 5
      )
      
      response <- GET("https://clinicaltrials.gov/api/v2/studies",
                     query = simple_params,
                     timeout(30))
      
      return(list(
        success = !http_error(response),
        status = status_code(response),
        message = if(http_error(response)) "API connection failed" else "API connection successful"
      ))
    }, error = function(e) {
      return(list(
        success = FALSE,
        status = "error",
        message = paste("Connection test failed:", e$message)
      ))
    })
  }
  
  # Test button functionality
  observeEvent(input$testBtn, {
    # Test API connection
    test_result <- test_api_connection()
    if(test_result$success) {
      showNotification("✓ API connection successful!", type = "message") # Changed type to "message"
    } else {
      # Display more informative error message
      error_detail <- ifelse(!is.null(test_result$status_code), 
                           paste("Status code:", test_result$status_code), 
                           "Check console for details.")
      showNotification(paste("✗ API connection failed.", error_detail, test_result$message), type = "error")
    }
  })
  
  return(list(
    data = reactive({
      df <- studies_data()
      if (is.null(df)) return(NULL)
      if (isTRUE(input$documentsOnly)) {
        df <- df[!is.na(df$docpath) & nzchar(df$docpath), ]
      }
      df
    }),
    info = search_info
  ))
}

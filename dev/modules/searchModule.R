# modules/searchModule.R
searchModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("query"), "Search Condition:", value = "cancer"),
    numericInput(ns("pageSize"), "Number of Studies:", value = 100, min = 1, max = 1000),
    actionButton(ns("searchBtn"), "Search", icon = icon("search"))
  )
}

searchModule <- function(input, output, session) {
  studies_data <- reactiveVal(NULL)
  fetch_page <- function(url, params) {
    response <- GET(url, query = params)
    if (http_error(response)) {
      stop(paste("API request failed with status code", status_code(response)))
    }
    content <- content(response, as = "text", encoding = "UTF-8")
    fromJSON(content, flatten = TRUE)
  }

  observeEvent(input$searchBtn, {
    withProgress(message = 'Fetching data...', {
      base_url <- "https://clinicaltrials.gov/api/v2/studies"
      query_params <- list(
        query.cond = input$query,
        fields = "NCTId,BriefTitle,OverallStatus,DocumentSection",
        format = "json",
        pageSize = input$pageSize
      )
      tryCatch({
        page_data <- fetch_page(base_url, query_params)
        studies <- page_data$studies |>
          tidyr::unnest(cols = c(documentSection.largeDocumentModule.largeDocs))

        root_largedocs <- "https://cdn.clinicaltrials.gov/large-docs"
        studies_updated <- studies |>
          mutate(docpath = ifelse(!is.na(filename),
                                  paste0(root_largedocs, "/",
                                         str_sub(protocolSection.identificationModule.nctId, -2), "/",
                                         protocolSection.identificationModule.nctId, "/",
                                         filename),
                                  NA))

        studies_data(studies_updated)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })
  return(studies_data)
}

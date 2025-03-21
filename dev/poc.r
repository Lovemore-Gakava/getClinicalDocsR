# Install and load necessary libraries
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("dplyr")) install.packages("dplyr")

library(httr)
library(jsonlite)
library(dplyr)

# Function to fetch studies from ClinicalTrials.gov API
fetch_studies <- function(query_cond = NULL, query_term = NULL, format = "json", page_size = 100) {
  # Base URL for the API
  base_url <- "https://clinicaltrials.gov/api/v2/studies"

  # Construct the query parameters
  params <- list(
    format = format,
    pageSize = page_size
  )

  # Add optional query parameters if provided
  if (!is.null(query_cond)) {
    params[["query.cond"]] <- query_cond
  }
  if (!is.null(query_term)) {
    params[["query.term"]] <- query_term
  }

  # Initialize an empty data frame to store the results
  all_studies <- data.frame()

  # Initialize the page token to NULL for the first request
  page_token <- NULL

  # Loop until there are no more pages
  repeat {
    # Add the page token to the parameters if it's not NULL
    if (!is.null(page_token)) {
      params[["pageToken"]] <- page_token
    }

    # Make the API request
    response <- GET(base_url, query = params)

    # Check for HTTP errors
    if (http_error(response)) {
      stop(paste("API request failed with status", status_code(response)))
    }

    # Parse the JSON response
    content <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content, flatten = TRUE)

    # Check if 'studies' exists in the response
    if (!"studies" %in% names(data) || length(data$studies) == 0) {
      message("No studies found or end of pages reached.")
      break
    }

    # Add studies to the data frame
    studies_df <- as.data.frame(data$studies)
    all_studies <- bind_rows(all_studies, studies_df)

    # Check if there is a next page token
    if (!is.null(data$nextPageToken)) {
      page_token <- data$nextPageToken
    } else {
      break
    }
  }

  return(all_studies)
}

# Example usage: Download studies related to "lung cancer" with a page size of 10
lung_cancer_studies <- fetch_studies(query_cond = "lung cancer", page_size = 10)

# Print the first few rows of the downloaded data
head(lung_cancer_studies)

# Example usage: Download studies related to "lung cancer" AND "smoking cessation"
lung_cancer_smoking_studies <- fetch_studies(query_cond = "lung cancer", query_term = "smoking cessation", page_size = 10)

# Example usage: Download studies related to "lung cancer" updated since 2023-01-15
lung_cancer_updated_studies <- fetch_studies(query_cond = "lung cancer", query_term = "AREA[LastUpdatePostDate]RANGE[2023-01-15,MAX]", page_size = 10)

lung_cancer_studies <- fetch_studies(query_cond = "lung cancer", page_size = 10)
lung_cancer_smoking_studies <- fetch_studies(query_cond = "lung cancer", query_term = "smoking cessation", page_size = 10)
lung_cancer_updated_studies <- fetch_studies(query_cond = "lung cancer", query_term = "AREA[LastUpdatePostDate]RANGE[2023-01-15,MAX]", page_size = 10)

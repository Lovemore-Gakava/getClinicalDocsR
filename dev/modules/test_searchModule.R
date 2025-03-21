library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

source("searchModule.R")  # Ensure this file exists

ui <- fluidPage(
  searchModuleUI("search"),
  DT::DTOutput("resultsTable")
)

server <- function(input, output, session) {
  studies_data <- callModule(searchModule, "search")

  output$resultsTable <- DT::renderDT({
    req(studies_data())
    datatable(studies_data())
  })
}

shinyApp(ui, server)

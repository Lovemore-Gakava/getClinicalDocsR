# ui.R
library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(DT)
library(stringr)

source("modules/searchModule.R")
source("modules/studiesModule.R")
source("modules/bookmarkModule.R")
source("modules/pdfViewerModule.R")
source("modules/downloadModule.R")

ui <- dashboardPage(
  dashboardHeader(title = "ClinicalTrials.gov Document Viewer"),
  dashboardSidebar(searchModuleUI("search")),
  dashboardBody(
    tabBox(
      width = 12,
      id = "mainTabs",
      tabPanel("Studies",
               actionButton("bookmarkBtn", "Bookmark Selected PDFs", icon = icon("bookmark")),
               actionButton("deselectBtn", "Deselect Selected PDFs", icon = icon("times")),
               studiesModuleUI("studies")
      ),
      tabPanel("Bookmarked PDFs", bookmarkModuleUI("bookmarks"))
    )
  )
)

# server.R
server <- function(input, output, session) {
  studies_data <- callModule(searchModule, "search")
  selected_rows <- callModule(studiesModule, "studies", studies_data)
  bookmarked_pdfs <- callModule(bookmarkModule, "bookmarks", studies_data, selected_rows)
  callModule(pdfViewerModule, "pdfViewer", selected_rows)
  callModule(downloadModule, "download", selected_rows, bookmarked_pdfs)

  observeEvent(input$bookmarkBtn, {
    session$sendCustomMessage("bookmark", list(action = "add"))
  })

  observeEvent(input$deselectBtn, {
    session$sendCustomMessage("bookmark", list(action = "remove"))
  })
}

shinyApp(ui, server)

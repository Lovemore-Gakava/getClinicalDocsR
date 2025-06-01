# ui.R
library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(DT)
library(stringr)
library(shinyjs)  # required for highlighting
library(zip)

source("modules/searchModule.R")
source("modules/studiesModule.R")
source("modules/bookmarkModule.R")
source("modules/pdfViewerModule.R")
source("modules/downloadModule.R")

ui <- dashboardPage(
  dashboardHeader(title = "ClinicalTrials.gov Document Viewer"),
  dashboardSidebar(
    width = 350,  # Increased width for better spacing
    searchModuleUI("search")
  ),
  dashboardBody(
    useShinyjs(),
    # Add custom CSS for better styling
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          margin-left: 350px;
        }
        
        .main-sidebar {
          width: 350px;
        }
        
        .sidebar {
          height: 100vh;
          overflow-y: auto;
        }
        
        @media (max-width: 767px) {
          .content-wrapper, .right-side {
            margin-left: 0;
          }
        }
        
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top: 3px solid #3c8dbc;
        }

        /* Reasonable readable DataTable styles for all tables */
        table.dataTable {
          font-size: 15px !important;
          border-spacing: 0 !important;
          border-collapse: collapse !important;
        }
        table.dataTable tbody td,
        table.dataTable tbody th,
        table.dataTable thead th,
        table.dataTable thead td {
          padding: 6px 12px !important;
          line-height: 18px !important;
          min-height: 0 !important;
          vertical-align: top !important;
          white-space: nowrap !important;
        }
        table.dataTable tr {
          min-height: 0 !important;
        }
      "))
    ),
    
    tabBox(
      width = 12,
      id = "mainTabs",
      tabPanel("Studies",
               icon = icon("table"),
               fluidRow(
                 column(12,
                   div(style = "margin-bottom: 50px;",
                     bookmarkActionsUI("bookmarks")
                   ),
                   studiesModuleUI("studies")
                 )
               )
      ),
      tabPanel("Bookmarked PDFs", 
               icon = icon("bookmark"),
               bookmarkModuleUI("bookmarks")
      )
    )
  )
)

# server.R
server <- function(input, output, session) {
  search_results <- callModule(searchModule, "search")
  studies_data <- reactive({ search_results$data() })
  selected_rows <- callModule(studiesModule, "studies", studies_data)
  bookmarked_pdfs <- callModule(bookmarkModule, "bookmarks", studies_data, selected_rows)
  callModule(pdfViewerModule, "pdfViewer", selected_rows)
  callModule(downloadModule, "download", selected_rows, bookmarked_pdfs)
}

shiny::shinyApp(ui, server)

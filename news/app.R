library(tidyverse)
library(httr)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(devtools)
#install_github("nik01010/dashboardthemes")

# load api helper functions
source("api_wrappers.R")

# user interface
ui <- fluidPage(
  dashboardPage(
  dashboardHeader(title = "News Hub"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("User Guide", tabName = "userguide", 
               icon = icon("book-open")),
      menuItem("Top Headlines", tabName = "topheadlines", 
               icon = icon("search")),
      menuItem("Sources", tabName = "sources", 
               icon = icon("newspaper")),
      menuItem("Links", tabName = "links", 
               icon = icon("link"))
    )
  ),
  dashboardBody(
    ### changing theme
    shinyDashboardThemes(
      theme = "flat_red"
    ),
    # change header font
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Montserrat";
        text-transform: uppercase;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
    tabItems(
      # user guide tab
      tabItem(tabName = "userguide",
              h2("User guide tab content")
      ),
      
      # top headlines tab
      tabItem(tabName = "topheadlines",
              h2("Top headlines tab content")
      ),
      
      # sources tab
      tabItem(tabName = "sources",
              h2("Sources tab content")
      ),
      
      # links tab
      tabItem(tabName = "links",
              h2("Links tab content")
      )
    )
  )
))

# server function
server <- function(input, output) {


    
}

# run the application 
shinyApp(ui = ui, server = server)

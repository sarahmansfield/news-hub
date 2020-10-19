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
      ### apply theme
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
                h2("Top headlines tab content"),
                sidebarLayout(
                  sidebarPanel(h4(strong("Search options:")),
                    # inputs/outputs
                    radioButtons(inputId = "country",
                              label   = h5("Location:"),
                              choices = c("USA", "Worldwide")),
                    selectInput(inputId = "category",
                                label   = h5("Category:"),
                                choices = c("", "Business", "Entertainment",
                                            "General", "Health", "Science",
                                            "Sports", "Technology")),
                    textInput(inputId = "sources",
                              label   = h5("Sources:")),
                    textInput(inputId = "q",
                              label   = h5("Keywords:")),
                    numericInput(inputId = "pageSize",
                                 label = h5("Number of results to 
                                 return per page:"),
                                 value = 0,
                                 min = 0,
                                 max = 100),
                    numericInput(inputId = "page",
                                 label = h5("Page number:"),
                                 value = 0,
                                 min = 0),
                    textInput(inputId = "apiKey",
                              label   = h5("API Key:")),
                    # action button
                    div(align = "right",
                        actionButton(inputId = "getdata",
                                     label   = strong("Search"))
                    )
                  ),
                  mainPanel(
                    # inputs/outputs
                    
                  )
                )
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
  )
)

# server function
server <- function(input, output) {


    
}

# run the application 
shinyApp(ui = ui, server = server)

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
  useShinyalert(),
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
                    textInput(inputId = "q",
                              label   = h5("Keywords:")),
                    numericInput(inputId = "pageSize",
                                 label = h5("Number of results to 
                                 return per page:"),
                                 value = 0,
                                 min = 0,
                                 max = 100),
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
                    box(
                      title = "Inputs", solidHeader = TRUE,
                      "Box content here", br(), "More box content",
                      sliderInput("slider", "Slider input:", 1, 100, 50),
                      textInput("text", "Text input:")
                    )
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
  observeEvent(input$getdata, {
    if (input$pageSize < 0 | input$pageSize > 100) {
      shinyalert(title = "ERROR",
                 text  = "The number of results to return per page must be 
                 between 0 and 100",
                 type  = "error")
    }
    if (input$apiKey == "") {
      shinyalert(title = "ERROR",
                 text  = "You must enter in an API key",
                 type  = "error")
    }
  })
  # pull data
  observeEvent(input$getdata, {
    if (input$country == "USA") {
      data <- eventReactive(input$getdata, {
        get_top_headlines(country = "us", category = input$category, 
                          q = input$q, pageSize = input$pageSize,
                          apiKey = input$apiKey)
      })
    } else {
      data <- eventReactive(input$getdata, {
        get_top_headlines(category = input$category, q = input$q, 
                          pageSize = input$pageSize, apiKey = input$apiKey)
      })
    }
  })
  
    
}

# run the application 
shinyApp(ui = ui, server = server)

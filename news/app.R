library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinydashboard)
library(dashboardthemes)
library(devtools)
#install_github("nik01010/dashboardthemes")

# load api helper functions
source("api_wrappers.R")

# user interface
ui <- fluidPage(
  shinyjs::useShinyjs(),
  #js function to reset a button, variableName is the button name whose value we want to reset
  tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                Shiny.onInputChange(variableName, null);
                });
                "),
  uiOutput("modal"),
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
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
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
                                 label = h5("Number of results to return:"),
                                 value = 20,
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
                    # output news articles
                    # output data table
                    DT::dataTableOutput(outputId = "toparticles")
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
server <- function(input, output, session) {
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  observeEvent(input$getdata, {
    if (input$pageSize < 0 | input$pageSize > 100) {
      shinyalert(title = "ERROR",
                 text  = "The number of results to return must be 
                 between 0 and 100",
                 type  = "error")
    }
    if (input$apiKey == "") {
      shinyalert(title = "ERROR",
                 text  = "You must enter in an API key",
                 type  = "error")
    }
  })
  data <- eventReactive(input$getdata, {
    if (input$country == "USA") {
      countryinput <- "us"
    } else {
      countryinput <- ""
    }
    get_top_headlines(country = countryinput, category = input$category, 
                      q = input$q, pageSize = input$pageSize,
                      apiKey = input$apiKey)
  })
  # output data table
  output$toparticles <- DT::renderDataTable({
    articles <- data() %>%
      mutate(title = str_c("<a href='", url, "'>", title, "</a>")) %>%
      dplyr::select(Title = title, Author = author, Source = sourceName, 
                    Date = publishDate) %>%
      mutate(Date = ymd_hms(Date)) %>%
      arrange(desc(Date))
    articles$Preview = shinyInput(actionButton, nrow(articles), 'button_', 
                                  label = "Preview", 
                                  onclick = 'Shiny.onInputChange(\"preview_button\", this.id)')
    articles$`Sentiment Analysis` = shinyInput(actionButton, nrow(articles), 'button_', 
                                               label = "Sentiment Analysis", 
                                               onclick = 'Shiny.onInputChange(\"sa_button\", this.id)')
    datatable(articles, escape = FALSE, rownames = FALSE)
  })
  # popup preview box
  observeEvent(input$preview_button, {
    s <- as.numeric(strsplit(input$preview_button, "_")[[1]][2])
    output$modal <- renderUI({
      tagList(
        bsModal(paste('model', s ,sep=''), "Preview", "preview_button", size = "small",
                textAreaInput("text", label = h3("Enter Assessment") , value = "", width = "100%", height = "200px", resize = "none"),
                actionButton("Enter", "Enter")
        ))
    })
    toggleModal(session,paste('model', s ,sep=''), toggle = "Preview")
    ##Reset the select_button
    session$sendCustomMessage(type = 'resetInputValue', message =  "preview_button")
  })    
  # sentiment analysis
  observeEvent(input$sa_button, {
    s <- as.numeric(strsplit(input$sa_button, "_")[[1]][2])
    output$modal <- renderUI({
      tagList(
        bsModal(paste('model', s ,sep=''), "Sentiment Analysis", "sa_button", size = "small",
                textAreaInput("text", label = h3("Enter Assessment") , value = "", width = "100%", height = "200px", resize = "none"),
                actionButton("Enter", "Enter")
        ))
    })
    toggleModal(session,paste('model', s ,sep=''), toggle = "Sentiment Analysis")
    ##Reset the select_button
    session$sendCustomMessage(type = 'resetInputValue', message =  "sa_button")
  })  
}

# run the application 
shinyApp(ui = ui, server = server)

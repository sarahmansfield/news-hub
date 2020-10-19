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
  #js function to reset a button
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
                    # output news articles data table
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
  # pull top headlines data
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
    articles$`Sentiment Analysis` = shinyInput(actionButton, nrow(articles), 'button_', 
                                               label = "Sentiment Analysis", 
                                               onclick = 'Shiny.onInputChange(\"sa_button\", this.id)')
    datatable(articles, escape = FALSE, rownames = FALSE)
  })
  # build sentiment analysis popup window
  observeEvent(input$sa_button, {
    if (length(input$toparticles_rows_selected) != 1) {
      shinyalert(title = "ERROR",
                 text  = "You must select exactly one row at a time in order to 
                 conduct a sentiment analysis",
                 type  = "error")
    }
    s <- as.numeric(strsplit(input$sa_button, "_")[[1]][2])
    output$modal <- renderUI({
      tagList(
        bsModal(paste('model', s ,sep=''), "Sentiment Analysis", "sa_button", size = "large",
                sidebarLayout(
                  # input options
                  sidebarPanel(
                    radioButtons(inputId = "SAtype", 
                                 label = h5(strong("Conduct a sentiment analysis on this article's:")),
                                 choices = c("Title", "Description")),
                    # action button
                    actionButton(inputId = "getSAdata",
                                 label   = "Enter")
                  ),
                  # sentiment analysis output
                  mainPanel(
                    h5(strong("Title:")),
                    textOutput(outputId = "SAtitle"),
                    h5(strong("Description:")),
                    textOutput(outputId = "SAdesc"),
                    br(),
                    h5(strong("Results:")),
                    h5(textOutput(outputId = "sentimentanalysis"))
                    )
                  )
                )
        )
    })
    toggleModal(session,paste('model', s, sep=''), toggle = "Sentiment Analysis")
    ##Reset the select_button
    session$sendCustomMessage(type = 'resetInputValue', message =  "sa_button")
  })  
  # pull sentiment analysis data
  SAdata <- eventReactive(input$getSAdata, {
    index <- input$toparticles_rows_selected #index of the current row
    if (input$SAtype == "Title") {
      x <- data() %>%
        filter(row_number() == index) %>%
        select(title) %>%
        pull()
    } else {
      x <- data() %>%
        filter(row_number() == index) %>%
        select(description) %>%
        pull()
    }
    get_sentim(x)
  })
  # article title (for sentiment analysis)
  output$SAtitle <- renderText({
    index <- input$toparticles_rows_selected #index of the current row
    title <- data() %>%
      filter(row_number() == index) %>%
      select(title) %>%
      pull()
    title
  })
  # article description (for sentiment analysis)
  output$SAdesc <- renderText({
    index <- input$toparticles_rows_selected #index of the current row
    description <- data() %>%
      filter(row_number() == index) %>%
      select(description) %>%
      pull()
    description
  })
  # output sentiment analysis results
  output$sentimentanalysis <- renderText({
    overallSA <- SAdata() %>%
      slice(1) %>%
      select(overallPolarity, overallType)
    str_c("The text is ", overallSA$overallType, 
          " and has an overall polarity of ", overallSA$overallPolarity, ".")
  })
}

# run the application 
shinyApp(ui = ui, server = server)

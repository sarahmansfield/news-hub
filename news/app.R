library(tidyverse)
library(lubridate)
library(cowplot)
library(magick)
library(httr)
library(jsonlite)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinydashboard)
library(dashboardthemes)
library(DT)
library(devtools)
#install_github("nik01010/dashboardthemes")

# load api helper functions
source("api_wrappers.R")

# user interface
ui <- fluidPage(
  # change header font
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$style("body {font-family: 'Montserrat';}")
  ),
  shinyjs::useShinyjs(),
  #js function to reset a button
  tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                Shiny.onInputChange(variableName, null);
                });
                "),
  useShinyalert(),
  
  # sentiment analysis modal window
  bsModal(id = "saModal", 
          title = "Article Preview", 
          trigger = "sa_button", 
          size = "large",
          plotOutput(outputId = "articleimage"),
          h5(strong("Content Preview:")),
          textOutput(outputId = "articlecontent"),
          br(),
          sidebarLayout(
            # input options
            sidebarPanel(
              radioButtons(inputId = "SAtype", 
                           label = h5(strong("Conduct a sentiment analysis on this article's:")),
                           choices = c("Title", "Description")),
              # action button
              actionButton(inputId = "getSAdata",
                           label   = "Analyze"),
              # reset/clear button
              actionButton(inputId = "reset",
                           label   = "Clear")
            ),
            # sentiment analysis output
            mainPanel(
              # conditional panel
              conditionalPanel(condition = "input.SAtype == 'Title'",
                               h5(strong("Title:")),
                               textOutput(outputId = "articletitle")
              ),
              # conditional panel
              conditionalPanel(condition = "input.SAtype == 'Description'",
                               h5(strong("Description:")),
                               textOutput(outputId = "articledesc")
              ),
              br(),
              h5(strong("Results:")),
              textOutput(outputId = "sentimentanalysis")
            )
          )
  ),
  
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
      tabItems(
        # user guide tab
        tabItem(tabName = "userguide",
                h2("User guide tab content")
        ),
        
        # top headlines tab
        tabItem(tabName = "topheadlines",
                h2("Search For Top News Headlines"),
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
                              value = "c83d17a6e96f479ea20604bc27802a2d",
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
                h2("Search For News Sources"),
                sidebarLayout(
                  sidebarPanel(h4(strong("Search options:")),
                               # inputs/outputs
                               radioButtons(inputId = "country2",
                                            label   = h5("Location:"),
                                            choices = c("USA", "Worldwide")),
                               selectInput(inputId = "category2",
                                           label   = h5("Category:"),
                                           choices = c("", "Business", "Entertainment",
                                                       "General", "Health", "Science",
                                                       "Sports", "Technology")),
                               textInput(inputId = "apiKey2",
                                         value = "c83d17a6e96f479ea20604bc27802a2d",
                                         label   = h5("API Key:")),
                               # action button
                               div(align = "right",
                                   actionButton(inputId = "getsources",
                                                label   = strong("Search"))
                               )
                  ),
                  mainPanel(
                    # output news sources data table
                    DT::dataTableOutput(outputId = "sources")
                  )
                )
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
    if (input$country == "Worldwide" & 
        all(list(input$category, input$q) == "")) {
      shinyalert(title = "ERROR",
                 text  = "To search for worldwide news, at least one of the 
                 following parameters must also be specified: Category/Keywords",
                 type  = "error")
    }
    if (input$pageSize < 0 | input$pageSize > 100) {
      shinyalert(title = "ERROR",
                 text  = "The number of results to return must be between 0 and 100",
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
    if (nrow(data()) > 0) {
      articles <- data() %>%
      mutate(title = str_c("<a href='", url, "'>", title, "</a>")) %>%
      dplyr::select(Title = title, Author = author, Source = sourceName, 
                    Date = publishDate) %>%
      mutate(Date = ymd_hms(Date)) %>%
      arrange(desc(Date))
      articles$Preview = shinyInput(actionButton, nrow(articles), 'button_', 
                                               label = "Preview", 
                                               onclick = 'Shiny.onInputChange(\"sa_button\", this.id)')
      datatable(articles, escape = FALSE, rownames = FALSE, selection = 'single') %>% 
        formatDate(4, "toLocaleString")
    } else {
      shinyalert(text  = "No results found. Please try another search.",
                 type  = "info")
      datatable(data(), colnames = "No results found")
    }
  })
  
  # article title
  output$articletitle <- renderText({
    index <- input$toparticles_rows_selected #index of the current row
    if (length(index) > 0) {
      title <- data() %>%
        filter(row_number() == index) %>%
        select(title) %>%
        pull()
      title
    } else {
      stop("A row must be selected in order to generate data on this article. Please go back and ensure that a row in the data table is selected when the button is clicked.")
    }
    
  })
  # article description
  output$articledesc <- renderText({
    index <- input$toparticles_rows_selected #index of the current row
    if (length(index) > 0) {
      description <- data() %>%
        filter(row_number() == index) %>%
        select(description) %>%
        pull()
      description
    } else {
      stop("A row must be selected in order to generate data on this article. Please go back and ensure that a row in the data table is selected when the button is clicked.")
    }
    
  })
  # article image
  output$articleimage <- renderPlot({
    index <- input$toparticles_rows_selected #index of the current row
    if (length(index) > 0) {
      imageurl <- data() %>%
        filter(row_number() == index) %>%
        select(image) %>%
        pull()
      ggdraw() + draw_image(imageurl)
    } else {
      stop("A row must be selected in order to generate data on this article. Please go back and ensure that a row in the data table is selected when the button is clicked.")
    }
    
  })
  # article content
  output$articlecontent <- renderText({
    index <- input$toparticles_rows_selected #index of the current row
    if (length(index) > 0) {
      content <- data() %>%
        filter(row_number() == index) %>%
        select(content) %>%
        pull()
      if (is.na(content)) {
        content <- "No preview available for this article's contents"
      } else {
        # remove the [+ __ chars] portion of content
        content <- substr(content, 1, nchar(content) - 14)
      }
      content
    } else {
      stop("A row must be selected in order to generate data on this article. Please go back and ensure that a row in the data table is selected when the button is clicked.")
    }
  })
  
  observeEvent(input$sa_button, {
    toggleModal(session, "saModal", toggle = "toggle")
    ##Reset the sa_button
    session$sendCustomMessage(type = 'resetInputValue', message = "sa_button")
  })
  # pull sentiment analysis data
  SAdata <- eventReactive(input$getSAdata, {
    index <- input$toparticles_rows_selected #index of the current row
    if (length(index) > 0) {
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
    } else {
      stop("A row must be selected in order to generate data on this article. Please go back and ensure that a row in the data table is selected when the button is clicked.")
    }
    
  })
  # output sentiment analysis results
  observeEvent(input$getSAdata, {
    output$sentimentanalysis <- renderText({
      overallSA <- SAdata() %>%
        slice(1) %>%
        select(overallPolarity, overallType)
      str_c("This text is ", overallSA$overallType, 
            " and has an overall polarity of ", overallSA$overallPolarity, ".")
    })
  })
  # clears the output when reset button is clicked
  observeEvent(input$reset, {
    output$sentimentanalysis <- renderText({
      
    })
  })
  
  observeEvent(input$getsources, {
    if (input$apiKey2 == "") {
      shinyalert(title = "ERROR",
                 text  = "You must enter in an API key",
                 type  = "error")
    }
  })
  # pull sources data
  sourcesdata <- eventReactive(input$getsources, {
    if (input$country2 == "USA") {
      countryinput <- "us"
    } else {
      countryinput <- ""
    }
    get_sources(category = input$category2, country = countryinput, 
                apiKey = input$apiKey2)
  })
  # output data table
  output$sources <- DT::renderDataTable({
    if (nrow(sourcesdata()) > 0) {
      sources.dt <- sourcesdata() %>%
      mutate(sourceName = str_c("<a href='", url, "'>", sourceName, "</a>")) %>%
      dplyr::select(Source = sourceName, Description = description, 
                    Category = category, Country = country)
      datatable(sources.dt, escape = FALSE, rownames = FALSE, selection = 'single')
    } else {
      shinyalert(text  = "No results found. Please try another search.",
                 type  = "info")
      datatable(sourcesdata(), colnames = "No results found")
    }
  })
}

# run the application 
shinyApp(ui = ui, server = server)

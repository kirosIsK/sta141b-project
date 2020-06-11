library(shiny)
library(shinythemes)
library(tidyverse)
library(png)
library(jsonlite)
library(lubridate) 
library(httr)
library(rvest)

# Function to get Top Stories API
TOP_STORIES_API <- function(section) {
    web <- GET(
        str_glue("https://api.nytimes.com/svc/topstories/v2/{SECTION}.json?api-key={KEY}",
                 SECTION = section,
                 KEY     = Sys.getenv("NYT_KEY"))
    )
    
    u <- content(web, as = "text", encoding = "UTF-8")
    fromJSON(u, flatten = TRUE)
}


## Choice for session
TOP_STOREIS_SESSION_CHOICE <- c("arts","home","science","us","world")
sessionAPI = TOP_STORIES_API("US")


##-- Ui
ui <- fluidPage(
    theme = shinytheme("darkly"),
    titlePanel("NYTimes Top Stories"),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6, selectInput(
                    inputId  = "session",
                    label    = h5("Sessions"),
                    choices  = TOP_STOREIS_SESSION_CHOICE,
                    selected = "Select Session"))
                ),
            
            fluidRow(
                column(6, selectInput(
                    inputId  = "article",
                    label    = h5("Article"),
                    choices  = Choice$sessionAPI$result$title,
                    selected = "Select Article"))
                ),
            
            fluidRow(4, 
                h4(Choice$sessionAPI$result$title),
                verbatimTextOutput(outputId = "sessionInf")
            )
        ),
        mainPanel(
            htmlOutput("frame"),
            h4("Article Information"),
            verbatimTextOutput(outputId = "articleInf"),
        )
    )
)


##-- server
server <- function(input, output, session) {
    Choice <- reactiveValues(
        session    <- "US",
        sessionAPI <- TOP_STORIES_API(session),
        articleNo  <<- 1
    )
    
    cat(sprintf("Related Topic: %s"), res$des-dacet[[articleNo]])
    print(res$abstract[articleNo])
    
    ##-- Observe change of session
    observeEvent(input$session, {Choice$session    <- input$session; Choice$sessionAPI <- TOP_STORIES_API(input$session)})
    
    ##-- Observe change of article
    observeEvent(input$article, {Choice$articleNo  <- match(input$article, Choice$sessionAPI$results$title)})

    
    ##-- Print Article Information
    output$sessionInf <- renderPrint({
        # result of the API
        res <- Choice$sessionAPI$results
        
        # Subsection of this Article
        cat(sprintf("Related Topic: %s"), res$des-dacet[[articleNo]])

        # Last Updated of this Article
        cat(sprintf("Last Update of This Article: %s", res$updated_date[articleNo]))
        
        ## Print if there is any extra information
        ifelse(res$abstract[articleNo] != "", print(res$abstract[articleNo]), do <- "nothing")
    })
    
    
    ##-- Brwose Article Web
    output$frame <- renderUI({
        browserFrame <- tags$iframe(src=Choice$sessionAPI$results$url[articleNo], height=700, width=650)
        print(browserFrame)
        browserFrame
    })
}


shinyApp(ui, server)

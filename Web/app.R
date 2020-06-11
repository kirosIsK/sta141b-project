library(shiny)
library(shinythemes)
library(tidyverse)
library(png)
library(jsonlite)
library(lubridate) 
library(httr)
library(rvest)
library(scales)
library(ggplot2)
library(ggrepel)


library(gridExtra)
# Function to get Top Stories API
TOP_STORIES_API <- function(section) {
    web <- GET(
        str_glue("https://api.nytimes.com/svc/topstories/v2/{SECTION}.json?api-key={KEY}",
                 SECTION = section,
                 KEY     = "I74glzbxm6rd5lMhcqDATnUwvIufXuAb")
    )
    
    u <- content(web, as = "text", encoding = "UTF-8")
    fromJSON(u, flatten = TRUE)
}

## Choice for session
TOP_STOREIS_SESSION_CHOICE <- c("us","arts", "home", "science", "world")
sessionAPI <- TOP_STORIES_API("us")
articleNo  <- 1
    
## initial 
sub_section  <- sessionAPI$results$des_facet[[articleNo]]
updated_date <- sessionAPI$results$updated_date[articleNo]
extra_inf    <- sessionAPI$results$abstract[articleNo]

## Create a Blank Theme
blank_theme <- theme_minimal()+
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
    )

##-- Ui
ui <- fluidPage(
    shinythemes::themeSelector(),
    titlePanel("NYTimes Top Stories"),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6, selectInput(
                    inputId  = "session",
                    label    = h5("Session"),
                    choices  = TOP_STOREIS_SESSION_CHOICE,
                    selected = "Select Session"))
                ),
            
            fluidRow(
                column(6, selectInput(
                    inputId  = "article",
                    label    = h5("Article"),
                    choices  = sessionAPI$results$title,
                    selected = "Select Article"))
                ),
            fluidRow(
                h3("All information of the Article"),
                htmlOutput("lineBreak1")
            ),

            fluidRow(
                h4("Related Place"),
                textOutput(outputId = "geo")
            ),

            fluidRow(
                h4("Related Subsection"),
                column(6, htmlOutput((outputId = "subsection")))
            ),
            
            fluidRow(
                h4("Related Celebrity"),
                column(6, htmlOutput((outputId = "human")))
            ),
            
            fluidRow(
                h4("Updated Times"),
                textOutput(outputId = "updatedDate"),
            ),

            fluidRow(
                h4("Extra Information"),
                textOutput(outputId = "abstract")
            ),
            
            fluidRow(
                h3("All Information on Current Session"),
                htmlOutput("lineBreak2")
            ),
            
            fluidRow(
                h4("Total Articles Available"),
                textOutput(outputId = "numArticles")
            ),
            
            fluidRow(
                h4("Hottest Topic Currently"),
                textOutput(outputId = "hotTopic")
            ),
            
            fluidRow(
                h4("Distribution on the Topic"),
                plotOutput("topicDistr")
            ),

            fluidRow(
                h4("Times of Appearance for Celebrity"),
                plotOutput("celeDistr")
            ),
            
            fluidRow(
                htmlOutput("lineBreak3"),
                h6("Copyright (c) 2020 The New York Times Company. All Rights Reserved."))
        ),
        mainPanel(
            fluidRow(
                htmlOutput("frame")
                )
        )
    )
)


##-- server
server <- function(input, output, session) {
    
    sessionAPI <- reactive({
        TOP_STORIES_API(input$session)
    })
    
    articleNo  <- reactive({
        match(input$article, sessionAPI()$results$title)
    })

    ##-- Observe change of Session
    observeEvent(input$session, {
        updateSelectInput(session, "article", choices = sessionAPI()$results$title)
    })
    
                 
    ##-- Print geographic information
    output$geo <- renderText({
        geo_inf <- sessionAPI()$results$geo_facet[[articleNo()]]
        ifelse(length(geo_inf) > 0, paste(geo_inf), paste(""))
    })
    
    ##-- Print Subsection of this Article
    output$subsection <- renderUI({
        # Subsection of this Articl
        sub <- sessionAPI()$results$des_facet[[articleNo()]]
        HTML(paste(sub,sep = '<br/>'))
        HTML(paste(sub,sep = '<br/>'))
    })
    
    ##-- Print related celebrity on this news
    output$human <- renderUI({
        # Subsection of this Articl
        sub <- sessionAPI()$results$per_facet[[articleNo()]]
        HTML(paste(sub,sep = '<br/>'))
        HTML(paste(sub,sep = '<br/>'))
    })
    
    ##-- Print updated_date
    output$updatedDate <- renderText({
        # Last Updated of this Article
        data <- cbind("  ", sessionAPI()$results$updated_date[articleNo()] %>% str_replace("T", " "))
        paste(data)
    })
    
    ##-- Print a seperating line
    output$lineBreak1 <- renderUI({
        HTML(paste("⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻", sep="<br/>"))
    })
    
    #-- Print an seperating line
    output$lineBreak2 <- renderUI({
        HTML(paste("⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻", sep="<br/>"))
    })
    
    #-- Print an seperating line
    output$lineBreak3 <- renderUI({
        HTML(paste("⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻⎻", sep="<br/>"))
    })
    
    ##-- Print Article Information
    output$abstract <- renderText({
        extra <- sessionAPI()$results$abstract[articleNo()]
        ifelse(extra != "", paste(extra), paste(""))
    })
    
    ##-- Print Total num of articles
    output$numArticles <- renderText({
        paste(sessionAPI()$num_results)
    })
    
    ##-- Print Hottest Topic 
    output$hotTopic <- renderText({
        # Get the topic distriution
        oj  <- sessionAPI()$results %>% group_by(subsection) %>% summarize(freq = n()) %>% arrange(desc(freq))
        hot <- oj %>% select(subsection) %>% pluck(1)
    
        ## print Hottest topic name, print other if "" is the hottest
        ifelse(hot != "", paste(hot), paste("Other"))
    })
    
    ##-- Plot a chart for distribution of topics
    output$topicDistr = renderPlot({
        # Get the topic distriution
        oj        <- sessionAPI()$results %>% group_by(subsection) %>% summarize(freq = n()) %>% arrange(desc(freq))
        total_num <- oj %>% select(freq) %>% sum()
        
        ## Am not sure if it is my code or renderPlot,
        ## but sometimes geom_label_repel just point badly....
        
        ## You can still see the color to classify
        ggplot(oj, aes(x=0, y=freq, fill=subsection)) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start=0) + 
            scale_fill_brewer("Blues") + 
            blank_theme +
            theme(axis.text.x=element_blank()) +
            geom_label_repel(aes(label = percent(freq/total_num)), size = 5, show.legend = F, nudge_x = 1) +
            guides(fill = guide_legend(title = "Group"))
    })
    
    ##-- Plot a chart for popular of celebrity
    output$celeDistr = renderPlot({
        # Get the celebrity distriution
        oj        <- sessionAPI()$results$per_facet %>% unlist() %>% table() %>% 
            as_tibble() %>% rename(name = '.')
        total_num <- oj %>% select(n) %>% sum()
        
        ## Am not sure if it is my code or renderPlot,
        ## but sometimes geom_label_repel just point badly....
        
        ## You can still see the color to classify
        ggplot(oj, aes(y=name, x=n, fill=name)) +
            geom_bar(width = 1, stat = "identity")
    })
    
        
    ## Brwose Article Web
    ##  NY Times is preventing us to browse its website tragedically
    ##  We have to use this method
    output$frame <- renderUI({
        HTML(readLines(sessionAPI()$results$url[articleNo()]))
    })
    
    
    
}


shinyApp(ui, server)

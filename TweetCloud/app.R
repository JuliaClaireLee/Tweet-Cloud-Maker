#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(devtools)
library(readr)
library(tidyverse)
library(mdsr)
require(devtools)
library(ggthemes)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(gridExtra)
library(rmarkdown)
library(wordcloud)
library(tm)
library(textdata)
library(rtweet) 
library(tidytext)
library(ggpubr) 
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
devtools::install_github("karthik/wesanderson")
library("wesanderson")
#install.packages("RCurl")
library(RCurl)
#install.packages("maps")
library(maps)
library(shiny)
require(ggplot2)
require(ggplot2)
require(ggforce)
require(devtools)
require(shinythemes)
searchtweet<-function(x){
    c<- search_tweets(x, n=20000, include_rts = FALSE)
    return(c)
}
geotweet<-function(x){
    c<- search_tweets(x,geocode = lookup_coords("usa"), n=20000, include_rts = FALSE)
    return(c)
}
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
                # Application title
                titlePanel("Twitter Analysis"),
                sidebarPanel(
                    textInput("obs1", "Key Word:", value = "#love", width = NULL, placeholder = NULL )
                    
                    
                ),
                mainPanel(
                    plotOutput("distPlot"),
                    plotOutput("Plot"),
                    plotOutput("Plot2")
                )
                
                )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    output$distPlot <- renderPlot({
        rt <- searchtweet(input$obs1)
        tidy_tweets <-  rt %>% # pipe data frame  # only include original tweets
            select(status_id, 
                   text)%>% # select variables of interest
            unnest_tokens(word, text) # splits column in one token per row format
        
        my_stop_words <- tibble( #construct a dataframe
            word = c(
                "https",
                "t.co",
                "rt",
                "amp",
                "rstats",
                "gt"
            ),
            lexicon = "twitter"
        )
        # Connect stop words
        all_stop_words <- stop_words %>%
            bind_rows(my_stop_words) # here we are connecting two data frames
        
        # Let's see if it worked
        view(all_stop_words)
        
        # Remove numbers
        no_numbers <- tidy_tweets %>%
            filter(is.na(as.numeric(word)))
        no_stop_words <- no_numbers  %>%
            anti_join(all_stop_words, by = "word")
    
        
     
        nrc <- get_sentiments("nrc") # get specific sentiment lexicons in a tidy format
        nrc_words <- no_stop_words %>%
            inner_join(nrc, by="word")
        pie_words<- nrc_words %>%
            group_by(sentiment) %>% # group by sentiment type
            tally %>% # counts number of rows
            arrange(desc(n)) # arrange sentiments in descending order based on frequency
       
      
        
        

        pal2 <- wes_palette("Zissou1",4)
        pie_words<-as.data.frame(pie_words)
        wordz<-nrc_words$word
        group <- nrc_words$sentiment
        basecolors <- rainbow(10) 
        colorlist <- basecolors[ match(group,unique(group)) ]
        wordcloud(wordz, max.words = Inf, scale = c(4.5,0.21913127975125),random.order=FALSE,rot.per=.15, colors=colorlist)
    })
    output$Plot <- renderPlot({
        rt <- searchtweet(input$obs1)
        ts_plot(rt)
        }) 
    output$Plot2 <- renderPlot({
        rt <- geotweet(input$obs1)
        rt <- lat_lng(rt)
        
        ## plot state boundaries
        par(mar = c(0, 0, 0, 0))
        maps::map("state", lwd = .25)
        
        ## plot lat and lng points onto state map
        with(rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(rtweet)
library(sentimentr)
library(ggplot2)
library(dplyr)
library(DT)

api_key <- "Xq3x4pFiPMyD82jjsBrcAx34N"
api_secret_key <- "loQhP3EnOuAF0qT3xluIbZsTRtSaAdLhjoI34NGrOymWs1RJ5Q"
bearer_token <- "AAAAAAAAAAAAAAAAAAAAAPMBIAEAAAAAfgM31WXH3o9Qf5y%2BjqqP4vdBiNw%3DTHDKGdVXdavWtAZisWPy5fMM4JwZHdJZbkZVFla0YorgALRtNq"
access_token <- "1056354318142578689-mH77U56HMNjYH70nhcgyGokrgPVkKQ"
access_token_secret <- "ktjj7nCxxjVu2oHQIV2zcT8NJkJYLOIdgh4m7LcJd0JbU"

server <- function(input, output) {
  searchResults <- eventReactive(input$search, {
    #browser()
    withProgress(message = "Getting Tweets...", 
                 value = 1, {
                   rtweet::create_token(app = "ND Project", consumer_key = api_key, consumer_secret = api_secret_key, 
                                        access_token = access_token, access_secret = access_token_secret
                   )
                   tweets <- stream_tweets(input$search_term, timeout = as.numeric(input$search_time))
                   tweets2 <- flatten(tweets)
                 }
                 )
  })
  
  output$tweetsDT <- renderDataTable({
    req(searchResults())
    DT::datatable(searchResults(),
                  rownames = FALSE)
  })
  
  output$tweetsVerifiedDT <- renderDataTable({
    verified <- searchResults() %>% 
      filter(verified == TRUE)
    req(nrow(verified) > 0)
    DT::datatable(verified,
                  rownames = FALSE)
  })
  
  sentimentinfo <- eventReactive(input$search, {
    req(searchResults())
    tweets <- searchResults()
    tweets$rowID <- 1:nrow(tweets)
    tweets$text <- tolower(tweets$text)
    
    # Loading lexicon 
    load(url(
      "https://raw.githubusercontent.com/saberry/courses/master/hash_sentiment_vadar.RData"))
    
    tweetsSentiment <- sentiment(tweets$text, polarity_dt = hash_sentiment_vadar)
    
    sentimentAll <- tweetsSentiment %>% 
      group_by(element_id) %>% 
      summarize(sentiment = mean(sentiment)) %>% 
      right_join(., tweets, by = c("element_id" = "rowID")) %>% 
      select(sentiment, created_at, element_id) %>% 
      mutate(ds = created_at, 
             y = sentiment) %>% 
      select(ds, y, element_id)
    sentimentAll
  })
  
  output$negativetext <- renderText({
    req(searchResults())
    "Most Negative Tweet:"
  })
  
  output$negativetweet <- renderText({
    req(searchResults())
    tweets <- searchResults()
    tweets$rowID <- 1:nrow(tweets)
    negative <- sentimentinfo()[sentimentinfo()$y == min(sentimentinfo()$y),]
    negative <- merge(negative, tweets, by.x = "element_id", by.y = "rowID")
    negative$text
  })
  
  output$numberNeg <- renderText({
    req(searchResults())
    tweets <- searchResults()
    tweets$rowID <- 1:nrow(tweets)
    negative <- sentimentinfo() %>% 
      filter(y < 0)
    negative <- merge(negative, tweets, by.x = "element_id", by.y = "rowID")
    paste("Number of Negative Tweets: ", as.character(nrow(negative)), sep = "")
  })
  
  output$numberPos <- renderText({
    req(searchResults())
    tweets <- searchResults()
    tweets$rowID <- 1:nrow(tweets)
    positive <- sentimentinfo() %>% 
      filter(y > 0)
    positive <- merge(positive, tweets, by.x = "element_id", by.y = "rowID")
    paste("Number of Positive Tweets: ", as.character(nrow(positive)), sep = "")
  })
  
  output$numberNoSocre <- renderText({
    req(searchResults())
    tweets <- searchResults()
    tweets$rowID <- 1:nrow(tweets)
    none <- sentimentinfo() %>% 
      filter(y == 0)
    none <- merge(none, tweets, by.x = "element_id", by.y = "rowID")
    paste("Number of Tweets Without Sentiment: ", as.character(nrow(none)), sep = "")
  })
  
  output$Positivetext <- renderText({
    req(searchResults())
    "Most Positive Tweet:"
  })
  
  output$verifiedTextDT <- renderText({
    req(searchResults())
    "Verified Tweets:"
  })
  
  output$TweetsTextDT <- renderText({
    req(searchResults())
    "All Tweets:"
  })
  output$positivetweet <- renderText({
    req(searchResults())
    tweets <- searchResults()
    tweets$rowID <- 1:nrow(tweets)
    positive <- sentimentinfo()[sentimentinfo()$y == max(sentimentinfo()$y),]
    positive <- merge(positive, tweets, by.x = "element_id", by.y = "rowID")
    positive$text
  })
  
  output$sentimentplot <- renderPlot({
    ggplot(sentimentinfo(), aes(ds, y)) + 
      geom_line() + 
      geom_point(size = .5) + 
      theme_minimal()
  })
  
  output$VerifiedText <- renderText({
    req(nrow(searchResults()) > 0)
    verifiedtweets <- nrow(searchResults() %>% 
                             filter(verified == TRUE))
    paste("Number of Verified Tweets: ", as.character(verifiedtweets), sep = "")
  })
  
  output$numberTweets <- renderText({
    req(nrow(searchResults()) > 0)
    tweets <- nrow(searchResults())
    paste("Number of Tweets: ", as.character(tweets), sep = "")
  })
  
  output$RetweetsText <- renderText({
    req(searchResults())
    retweets <- nrow(searchResults() %>% 
                       filter(is_retweet == TRUE))
    paste("Tweets that are Retweets: ", as.character(retweets), sep = "")
  })
  
}

ui <- fluidPage(
  fluidRow(
    #includeCSS("racetowhitehouse.css"),
    column(3, 
           br(),
           br(),
           tags$h3(tags$b(textOutput("negativetext"))),
           textOutput("negativetweet")), 
    column(6, 
           tags$h1("The Battle for 2020", align = "center"),
           tags$h4("This shiny dashboard grabs the streaming tweets for the two main presidential canidates in the 2020 election.", align = "center"),
           br(),
           tags$img(src = "both.jpg", 
                    style="display: block; margin-left: auto; margin-right: auto;", 
                    height = 250),
           tags$h4(tags$b(textOutput("numberTweets"))),
           tags$h4(tags$b(textOutput("RetweetsText"))),
           tags$h4(tags$b(textOutput("VerifiedText"))),
           tags$h4(tags$b(textOutput("numberPos"))),
           tags$h4(tags$b(textOutput("numberNeg"))),
           tags$h4(tags$b(textOutput("numberNoSocre")))), 
    column(3, 
           br(), 
           br(),
           tags$h3(tags$b(textOutput("Positivetext"))),
           textOutput("positivetweet"))),
  br(),
  fluidRow(
    column(4, 
           selectInput("search_term", 
                       "Search Term:",
                       c("Biden", 
                         "Democrats",
                         "Debate",
                         "Taxes",
                         "Trump",
                         "Republicans",
                         "RGB",
                         "Ruth Bader Ginsburg",
                         "Chris Wallace",
                         "$750"))),
    column(4, 
           selectInput("search_time",
                       "Search Time (in seconds)",
                       c(10, 20, 30, 40, 50, 60),
                       selected = 30)),
    column(4, 
           br(),
           actionButton("search",
                        "Search!",
                        class = "btn-success"))),
  fluidRow(
    plotOutput("sentimentplot")),
  fluidRow(
    tags$h3(tags$b(textOutput("verifiedTextDT")))),
  fluidRow(
    dataTableOutput("tweetsVerifiedDT")),
  fluidRow(
    tags$h3(tags$b(textOutput("TweetsTextDT")))),
  fluidRow(
    dataTableOutput("tweetsDT")
  ))

shinyApp(ui, server)
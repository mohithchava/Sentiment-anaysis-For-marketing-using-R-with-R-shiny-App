library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(tm)
library(slam) 

data <- fread("~/Desktop/MS ES DS/sem2/SDM2/clean_data Prj.csv")
pos_data <- data[data$sentiment == 'Positive']
neut_data <- data[data$sentiment == 'Neutral']
neg_data <- data[data$sentiment == 'Negative']

acc <- fread('~/Desktop/MS ES DS/sem2/SDM2/acc.csv')


ui <- fluidPage(
    titlePanel("Sentiment Analysis on product reviews for Marketing "),
    
    sidebarLayout(
        sidebarPanel(width = 3,
                     
                     selectInput("ml", 
                                 label = "ML model",
                                 choices = c("Logistic Regression", 
                                             "Decision Tree",
                                             "KNN", 
                                             "SVC",
                                             "Naive Bayes"),
                                 selected = "Logistic Regression"),
                     selectInput("dta", 
                                 label = "Deep Text Analysis",
                                 choices = c("Bigram Analysis", 
                                             "Wordcloud"),
                                 selected = "Bigram Analysis"),
                     selectInput("time", 
                                 label = "TimeFrame",
                                 choices = c("Year", 
                                             "Month",
                                             "Quarter"),
                                 selected = "Year"),
                     
                     conditionalPanel(
                         "input.time == 'Quarter'",
                         sliderInput("qtr", "Quarter", min = 1, max = 4, value = c(1,4)),
                         
                     ),
                     
                     conditionalPanel(
                         "input.time == 'Month'",
                         sliderInput("mnth", "Month", min = 1, max = 12, value = c(1,12)),
                         
                     ),
                     conditionalPanel(
                         "input.time == 'Year'",
                         sliderInput("yr", 
                                     label = "Year",
                                     min = 2004, max = 2014, value = c(2004, 2014), sep = "")
                     ),
                     
        ),
        
        
        mainPanel(
            textOutput("selected_var"),
            tabsetPanel(type = "tabs",
                        tabPanel("Deep Text Analysis",
                                 h4('Positive Reviews', align = 'center'),
                                 plotOutput('pos_txt'),
                                 fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), 
                                                 fluidRow(
                                                     h4('Neutral Reviews', align = 'center'),
                                                     plotOutput('neut_txt')
                                                 )
                                                 ,
                                                 fluidRow(
                                                     h4('Negative Reviews', align = 'center'),
                                                     plotOutput('neg_txt')
                                                 )
                                     )
                                 )),
                        tabPanel("Time vs Sentiment", 
                                 fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), 
                                                 fluidRow(
                                                     h4('Predicted values', align = 'center'),
                                                     plotOutput('line1')
                                                 )
                                                 ,
                                                 fluidRow(
                                                     h4('Actual Values', align = 'center'),
                                                     plotOutput('line2')
                                                 )
                                     )
                                 )
                        )
            )
        )
    )
)


server <- function(input, output) {
    tm <- reactive(input$time)
    
    output$selected_var <- renderText({ 
        if(input$ml == 'Logistic Regression'){
            a =round((acc$accuracy[1])*100, 2)
        }
        else if (input$ml == 'Decision Tree'){
            a = round((acc$accuracy[2])*100, 2)
        }
        else if (input$ml == 'KNN'){
            a = round((acc$accuracy[3])*100, 2)
        }
        else if(input$ml == 'SVC'){
            a = round((acc$accuracy[4])*100, 2)
        }
        else if(input$ml == 'Naive Bayes'){
            a = round((acc$accuracy[5])*100, 2)
        }
        paste("The accuracy of", input$ml, "is", a, "%")
        
    })
    
    
    output$line1 <- renderPlot(
        {
            if(input$ml == 'Logistic Regression'){
                if(input$time == 'Year'){
                    if(input$yr[1] == input$yr[2]){
                        year_data <- data[data$year == input$yr[1]]
                        year_data$month <- as.factor(year_data$month)
                        year_data <- year_data  %>%
                            group_by(y_log, month) %>%
                            summarize(count=n())
                        
                        ggplot(year_data, aes(x=month,y=count,group=y_log))+
                            geom_line(aes(color=y_log))
                    }
                    else {
                        year_data <- data[(data$year >= input$yr[1]) & (data$year<= input$yr[2])]
                        year_data$year <- as.factor(year_data$year)
                        year_data <- year_data %>%
                            group_by(y_log, year) %>%
                            summarize(count=n())
                        
                        ggplot(year_data, aes(x=year,y=count,group=y_log))+
                            geom_line(aes(color=y_log))
                    }
                    
                }
                
                
                else if (input$time == 'Month'){
                    month_data <- data[(data$month >= input$mnth[1]) & (data$month<= input$mnth[2])]
                    month_data$month <- as.factor(month_data$month)
                    month_data <- month_data %>%
                        group_by(y_log, month) %>%
                        summarize(count=n())
                    
                    ggplot(month_data, aes(x=month,y=count,group=y_log))+
                        geom_line(aes(color=y_log))
                }
                else{
                    quarter_data <- data[(data$quarter >= input$qtr[1]) & (data$quarter<= input$qtr[2])]
                    quarter_data$quarter <- as.factor(quarter_data$quarter)
                    quarter_data <- quarter_data %>%
                        group_by(y_log, quarter) %>%
                        summarize(count=n())
                    
                    ggplot(quarter_data, aes(x=quarter,y=count,group=y_log))+
                        geom_line(aes(color=y_log))
                }
            }
            
            else if (input$ml == 'Decision Tree'){
                if(input$time == 'Year'){
                    if(input$yr[1] == input$yr[2]){
                        year_data <- data[data$year == input$yr[1]]
                        year_data$month <- as.factor(year_data$month)
                        year_data <- year_data  %>%
                            group_by(y_dtree, month) %>%
                            summarize(count=n())
                        
                        ggplot(year_data, aes(x=month,y=count,group=y_dtree))+
                            geom_line(aes(color=y_dtree))
                    }
                    else {
                        year_data <- data[(data$year >= input$yr[1]) & (data$year<= input$yr[2])]
                        year_data$year <- as.factor(year_data$year)
                        year_data <- year_data %>%
                            group_by(y_dtree, year) %>%
                            summarize(count=n())
                        
                        ggplot(year_data, aes(x=year,y=count,group=y_dtree))+
                            geom_line(aes(color=y_dtree))
                    }
                    
                }
                
                
                else if (input$time == 'Month'){
                    month_data <- data[(data$month >= input$mnth[1]) & (data$month<= input$mnth[2])]
                    month_data$month <- as.factor(month_data$month)
                    month_data <- month_data %>%
                        group_by(y_dtree, month) %>%
                        summarize(count=n())
                    
                    ggplot(month_data, aes(x=month,y=count,group=y_dtree))+
                        geom_line(aes(color=y_dtree))
                }
                else{
                    quarter_data <- data[(data$quarter >= input$qtr[1]) & (data$quarter<= input$qtr[2])]
                    quarter_data$quarter <- as.factor(quarter_data$quarter)
                    quarter_data <- quarter_data %>%
                        group_by(y_dtree, quarter) %>%
                        summarize(count=n())
                    
                    ggplot(quarter_data, aes(x=quarter,y=count,group=y_dtree))+
                        geom_line(aes(color=y_dtree))
                }
            }
            else if (input$ml == 'KNN'){
                if(input$time == 'Year'){
                    if(input$yr[1] == input$yr[2]){
                        year_data <- data[data$year == input$yr[1]]
                        year_data$month <- as.factor(year_data$month)
                        year_data <- year_data  %>%
                            group_by(y_knn, month) %>%
                            summarize(count=n())
                        
                        ggplot(year_data, aes(x=month,y=count,group=y_knn))+
                            geom_line(aes(color=y_knn))
                    }
                    else {
                        year_data <- data[(data$year >= input$yr[1]) & (data$year<= input$yr[2])]
                        year_data$year <- as.factor(year_data$year)
                        year_data <- year_data %>%
                            group_by(y_knn, year) %>%
                            summarize(count=n())
                        
                        ggplot(year_data, aes(x=year,y=count,group=y_knn))+
                            geom_line(aes(color=y_knn))
                    }
                    
                }
                else if (input$time == 'Month'){
                    month_data <- data[(data$month >= input$mnth[1]) & (data$month<= input$mnth[2])]
                    month_data$month <- as.factor(month_data$month)
                    month_data <- month_data %>%
                        group_by(y_knn, month) %>%
                        summarize(count=n())
                    
                    ggplot(month_data, aes(x=month,y=count,group=y_knn))+
                        geom_line(aes(color=y_knn))
                }
                else{
                    quarter_data <- data[(data$quarter >= input$qtr[1]) & (data$quarter<= input$qtr[2])]
                    quarter_data$quarter <- as.factor(quarter_data$quarter)
                    quarter_data <- quarter_data %>%
                        group_by(y_knn, quarter) %>%
                        summarize(count=n())
                    
                    ggplot(quarter_data, aes(x=quarter,y=count,group=y_knn))+
                        geom_line(aes(color=y_knn))
                }
            }
            else if (input$ml == 'SVC'){
                if(input$time == 'Year'){
                    if(input$yr[1] == input$yr[2]){
                        year_data <- data[data$year == input$yr[1]]
                        year_data$month <- as.factor(year_data$month)
                        year_data <- year_data  %>%
                            group_by(y_svc, month) %>%
                            summarize(count=n())
                        
                        ggplot(year_data, aes(x=month,y=count,group=y_svc))+
                            geom_line(aes(color=y_svc))
                    }
                    else {
                        year_data <- data[(data$year >= input$yr[1]) & (data$year<= input$yr[2])]
                        year_data$year <- as.factor(year_data$year)
                        year_data <- year_data %>%
                            group_by(y_svc, year) %>%
                            summarize(count=n())
                        
                        ggplot(year_data, aes(x=year,y=count,group=y_svc))+
                            geom_line(aes(color=y_svc))
                    }
                    
                }
                
                
                else if (input$time == 'Month'){
                    month_data <- data[(data$month >= input$mnth[1]) & (data$month<= input$mnth[2])]
                    month_data$month <- as.factor(month_data$month)
                    month_data <- month_data %>%
                        group_by(y_svc, month) %>%
                        summarize(count=n())
                    
                    ggplot(month_data, aes(x=month,y=count,group=y_svc))+
                        geom_line(aes(color=y_svc))
                }
                else{
                    quarter_data <- data[(data$quarter >= input$qtr[1]) & (data$quarter<= input$qtr[2])]
                    quarter_data$quarter <- as.factor(quarter_data$quarter)
                    quarter_data <- quarter_data %>%
                        group_by(y_svc, quarter) %>%
                        summarize(count=n())
                    
                    ggplot(quarter_data, aes(x=quarter,y=count,group=y_svc))+
                        geom_line(aes(color=y_svc))
                }
            }
            else if (input$ml == 'Naive Bayes'){
                if(input$time == 'Year'){
                    if(input$yr[1] == input$yr[2]){
                        year_data <- data[data$year == input$yr[1]]
                        year_data$month <- as.factor(year_data$month)
                        year_data <- year_data  %>%
                            group_by(y_nb, month) %>%
                            summarize(count=n())
                        
                        ggplot(year_data, aes(x=month,y=count,group=y_nb))+
                            geom_line(aes(color=y_nb))
                    }
                    else {
                        year_data <- data[(data$year >= input$yr[1]) & (data$year<= input$yr[2])]
                        year_data$year <- as.factor(year_data$year)
                        year_data <- year_data %>%
                            group_by(y_nb, year) %>%
                            summarize(count=n())
                        
                        ggplot(year_data, aes(x=year,y=count,group=y_nb))+
                            geom_line(aes(color=y_nb))
                    }
                    
                }
                
            
                else if (input$time == 'Month'){
                    month_data <- data[(data$month >= input$mnth[1]) & (data$month<= input$mnth[2])]
                    month_data$month <- as.factor(month_data$month)
                    month_data <- month_data %>%
                        group_by(y_nb, month) %>%
                        summarize(count=n())
                    
                    ggplot(month_data, aes(x=month,y=count,group=y_nb))+
                        geom_line(aes(color=y_nb))
                }
                else{
                    quarter_data <- data[(data$quarter >= input$qtr[1]) & (data$quarter<= input$qtr[2])]
                    quarter_data$quarter <- as.factor(quarter_data$quarter)
                    quarter_data <- quarter_data %>%
                        group_by(y_nb, quarter) %>%
                        summarize(count=n())
                    
                    ggplot(quarter_data, aes(x=quarter,y=count,group=y_nb))+
                        geom_line(aes(color=y_nb))
                }
            }
            
            
            
            
        }
    )
    
    
    
    
    
    output$line2 <- renderPlot(
        {
            
            if(input$time == 'Year'){
                if(input$yr[1] == input$yr[2]){
                    year_data <- data[data$year == input$yr[1]]
                    year_data$month <- as.factor(year_data$month)
                    year_data <- year_data  %>%
                        group_by(sentiment, month) %>%
                        summarize(count=n())
                    
                    ggplot(year_data, aes(x=month,y=count,group=sentiment))+
                        geom_line(aes(color=sentiment))
                }
                else {
                    year_data <- data[(data$year >= input$yr[1]) & (data$year<= input$yr[2])]
                    year_data$year <- as.factor(year_data$year)
                    year_data <- year_data %>%
                        group_by(sentiment, year) %>%
                        summarize(count=n())
                    
                    ggplot(year_data, aes(x=year,y=count,group=sentiment))+
                        geom_line(aes(color=sentiment))
                }
                
            }
            
            
            else if (input$time == 'Month'){
                month_data <- data[(data$month >= input$mnth[1]) & (data$month<= input$mnth[2])]
                month_data$month <- as.factor(month_data$month)
                month_data <- month_data %>%
                    group_by(sentiment, month) %>%
                    summarize(count=n())
                
                ggplot(month_data, aes(x=month,y=count,group=sentiment))+
                    geom_line(aes(color=sentiment))
            }
            else{
                quarter_data <- data[(data$quarter >= input$qtr[1]) & (data$quarter<= input$qtr[2])]
                quarter_data$quarter <- as.factor(quarter_data$quarter)
                quarter_data <- quarter_data %>%
                    group_by(sentiment, quarter) %>%
                    summarize(count=n())
                
                ggplot(quarter_data, aes(x=quarter,y=count,group=sentiment))+
                    geom_line(aes(color=sentiment))
            }
        }
    )
    
    output$pos_txt <- renderPlot({
        if (input$time == 'Year'){
            pos_data <- pos_data[(pos_data$year >= input$yr[1]) & (pos_data$year<= input$yr[2])]
        }
        else if (input$time == 'Month'){
            pos_data <- pos_data[(pos_data$month >= input$mnth[1]) & (pos_data$month<= input$mnth[2])]
        }
        else{
            pos_data <- pos_data[(pos_data$quarter >= input$qtr[1]) & (pos_data$quarter<= input$qtr[2])]
        }
        
        if(input$dta == 'Bigram Analysis'){
            bgs <- pos_data %>% unnest_tokens(bigram, reviews, token = "ngrams", n = 2)
            bgc <- bgs %>% count(bigram, sort = TRUE)
            b<- head(bgc, 12)
            ggplot(b, aes(x = reorder(bigram, n), y = n)) + geom_bar(stat = 'identity') + 
                coord_flip() + xlab('') + ylab('') 
        }
        else {
            text <- pos_data$reviews
            docs <- Corpus(VectorSource(text))
            dtm <- TermDocumentMatrix(docs)
            mat <- as.matrix(dtm)
            words <- sort(rowSums(mat), decreasing = TRUE)
            df <- data.frame(word = names(words), freq = words)
            plot.new()
            text(x=0.5, y=0.5, "Title of my first plot")
            wordcloud(words = df$word, freq = df$freq, max.words = 75, 
                      colors=brewer.pal(8, "PuOr"))
            
        }    
        
    })
    
    output$neut_txt <- renderPlot({
        if (input$time == 'Year'){
            neut_data <- neut_data[(neut_data$year >= input$yr[1]) & (neut_data$year<= input$yr[2])]
        }
        else if (input$time == 'Month'){
            neut_data <- neut_data[(neut_data$month >= input$mnth[1]) & (neut_data$month<= input$mnth[2])]
        }
        else{
            neut_data <- neut_data[(neut_data$quarter >= input$qtr[1]) & (neut_data$quarter<= input$qtr[2])]
        }
        if(input$dta == 'Bigram Analysis'){
            bgs <- neut_data %>% unnest_tokens(bigram, reviews, token = "ngrams", n = 2)
            bgc <- bgs %>% count(bigram, sort = TRUE)
            b<- head(bgc, 12)
            #dev.off()
            ggplot(b, aes(x = reorder(bigram, n), y = n)) + geom_bar(stat = 'identity') + 
                coord_flip() + xlab('') + ylab('')
        }
        else{
            text <- neut_data$reviews
            docs <- Corpus(VectorSource(text))
            dtm <- TermDocumentMatrix(docs)
            mat <- as.matrix(dtm)
            words <- sort(rowSums(mat), decreasing = TRUE)
            df <- data.frame(word = names(words), freq = words)
            plot.new()
            text(x=0.5, y=0.5, "Title of my first plot")
            wordcloud(words = df$word, freq = df$freq, max.words = 75, 
                      colors=brewer.pal(8, "PuOr"))
            
        }    
    })
    
    output$neg_txt <- renderPlot({
        if (input$time == 'Year'){
            neg_data <- neg_data[(neg_data$year >= input$yr[1]) & (neg_data$year<= input$yr[2])]
        }
        else if (input$time == 'Month'){
            neg_data <- neg_data[(neg_data$month >= input$mnth[1]) & (neg_data$month<= input$mnth[2])]
        }
        else{
            neg_data <- neg_data[(neg_data$quarter >= input$qtr[1]) & (neg_data$quarter<= input$qtr[2])]
        }
        
        if(input$dta == 'Bigram Analysis'){
            bgs <- neg_data %>% unnest_tokens(bigram, reviews, token = "ngrams", n = 2)
            bgc <- bgs %>% count(bigram, sort = TRUE)
            b<- head(bgc, 12)
            #dev.off()
            ggplot(b, aes(x = reorder(bigram, n), y = n)) + geom_bar(stat = 'identity') + 
                coord_flip() + xlab('') + ylab('') 
            
        }
        else{
            text <- neg_data$reviews
            docs <- Corpus(VectorSource(text))
            dtm <- TermDocumentMatrix(docs)
            mat <- as.matrix(dtm)
            words <- sort(rowSums(mat), decreasing = TRUE)
            df <- data.frame(word = names(words), freq = words)
            wordcloud(words = df$word, freq = df$freq, max.words = 75, 
                      colors=brewer.pal(8, "PuOr"))
            
        }
    })
}


shinyApp(ui = ui, server = server)

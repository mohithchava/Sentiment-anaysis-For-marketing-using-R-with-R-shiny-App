install.packages('wordcloud')
install.packages('data.table')
install.packages('tm')
install.packages('slam')
library(data.table)
library(dplyr)
library(wordcloud)
library(tm)
library(slam) 

data<- fread('~/Desktop/MS ES DS/sem2/SDM2/clean_data.csv')
neg_data<- data[data$sentiment == 'Negative']
text <- neg_data$reviews
docs <- Corpus(VectorSource(text))
dtm <- TermDocumentMatrix(docs)
mat <- as.matrix(dtm)
words <- sort(rowSums(mat), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)

wordcloud(words = df$word, freq = df$freq, max.words = 200, colors=brewer.pal(8, "PuOr"))


if(input$time == 'Year'){
  if(input$yr[1] == input$yr[2]){
    year_data <- data[data$year == input$yr[1]]
    year_data$month <- as.factor(year_data$month)
    year_data <- year_data  %>%
      group_by(y_knn, month) %>%
      summarize(count=n())
    
    ggplot(month_data, aes(x=month,y=count,group=y_knn))+
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




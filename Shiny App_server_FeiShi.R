library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(stringr)
library(tidytext)
library(wordcloud)
library(broom)
library(data.table)
library(ggplot2)

#setwd("~/Desktop/MA 615 final project/shiny Trump/")
load("data/trump_tweets_df.rda")

tweets <- trump_tweets_df %>%
  select(id,  text, created) %>%
  mutate(Status = ifelse(created < "2016-6-6", "Before", "After"))



reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


tweet_words %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip()

beofre_after_ratios <- tweet_words %>%
  count(word, Status) %>%
  filter(sum(n) >= 5) %>%
  spread(Status, n, fill = 0) %>%
  ungroup() %>% 
  mutate(logratio = log2((Before +1) / (After+1))) %>%
  arrange(desc(logratio))



frequency <- tweet_words %>% 
  group_by(Status) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tweet_words %>% 
              group_by(Status) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)


frequency <- frequency %>% 
  select(Status, word, freq) %>% 
  spread(Status, freq) %>%
  arrange(Before, After)


temp =  tweet_words %>% group_by(Status) %>% 
  anti_join(stop_words) %>%
  count(word)



nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)


sources <- tweet_words %>%
  group_by(Status) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id, Status, total_words)



by_source_sentiment <- tweet_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  complete(sentiment, id, fill = list(n = 0)) %>%
  inner_join(sources) %>%
  group_by(Status, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup()





ms = c("hour","minute")

cis = c(0.90,0.95,0.99)

shinyServer(function(input, output) {
  
  
  output$f1 <-  renderPlot({   
   
  if(ms[as.integer(input$mini)] == "hour") {
    tweets %>%
      count(Status, hour = hour(with_tz(created, "EST"))) %>%
      mutate(percent = n / sum(n)) %>%
      ggplot(aes(hour, percent, color = Status)) +
      geom_line() +   labs(x = "Hour of day (EST)",
                           y = "% of tweets",
                           color = "")
  } else  if(ms[as.integer(input$mini)] == "minute") {
    tweets %>%
      count(Status, minute = minute(with_tz(created, "EST"))) %>%
      mutate(percent = n / sum(n)) %>%
      ggplot(aes(minute, percent, color = Status)) +
      geom_line() +   labs(x = "Minute of day (EST)",
                           y = "% of tweets",
                           color = "")
    }
    
  })
  
  output$f2 <-  renderPlot({ 
   
    temp2 = temp %>% filter(Status == "Before") %>% 
      with(wordcloud(word, n, max.words = as.integer(input$s2)))
   
    
  })
  
output$f3 <-  renderPlot({   
  temp3 = temp %>% filter(Status == "After")%>% 
    with(wordcloud(word, n, max.words = as.integer(input$s2)))
})


output$f4 <-  renderPlot({   
  beofre_after_ratios %>%
    group_by(logratio > 0) %>%
    top_n(as.integer(input$s3), abs(logratio)) %>%
    ungroup() %>%
    mutate(word = reorder(word, logratio)) %>%
    ggplot(aes(word, logratio, fill = logratio < 0)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Before / after log ratio") 
})

output$f5 <-  renderPlot({   
  
  ggplot(frequency, aes(Before, After)) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    geom_abline(color = "red")
  
})

output$tb1 <-  renderDataTable({   
  
  sentiment_differences <- by_source_sentiment %>%
    group_by(sentiment) %>%
    do(tidy(poisson.test(.$words, .$total_words,conf.level = cis[as.integer(input$ci)])))
  data.table(sentiment_differences)
  
})

output$f6 <-  renderPlot({   
  
  sentiment_differences <- by_source_sentiment %>%
    group_by(sentiment) %>%
    do(tidy(poisson.test(.$words, .$total_words,conf.level = cis[as.integer(input$ci)])))
  sentiment_differences %>%
    ungroup() %>%
    mutate(sentiment = reorder(sentiment, estimate)) %>%
    mutate_each(funs(. - 1), estimate, conf.low, conf.high) %>%
    ggplot(aes(estimate, sentiment)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
    scale_x_continuous(labels = percent_format()) +
    labs(x = "% increase in after relative to before",
         y = "Sentiment")
})


})




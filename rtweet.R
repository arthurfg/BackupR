install.packages("rtweet")
library(rtweet)
library(dplyr)
library(tidyr)
library(tidyverse)
## install httpuv if not already
install.packages("httpuv")
library(httpuv)
install.packages('tm')
library(tm)
install.packages("tidytext")
library(tidytext)  
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

library(SnowballC)
library(wordcloud)

install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)
  
  
  
  
app_name <- "rtweet_arthur_gusmao"
consumer_key <- "PTnIUgeoIJBaccldyHsFgxKWV"
consumer_secret <- "NWxcu8uFv9x6RlkdGQWRWQfhiNy5w69hhqFb0KC1dXPPi9OoZV"
access_token <- "1160975630206476288-z5GtRD2vri92oQrYfjTiD7GAclyHb2"
access_secret <- "v27y1WTlaRKnfXrpYuu94R6Grn2L5qi5fySlfQhx9TSvi"


token <- create_token(app = app_name,
             consumer_key = consumer_key,
             consumer_secret = consumer_secret)

econ_twitter <- search_tweets("#econtwitter",
                              n=100,
                              include_rts = F)

globo_news <- get_timeline('g1rio')

save_as_csv(head(econ_twitter),'econ_twitter.csv')
save_as_csv(head(globo_news),'g1.csv')


br <- trends_available(token = token)

trends_rj <- get_trends(woeid = 455825,
                        token = token)

bolsonaro <- search_tweets("#OPovoElegeuBolsonaro",
                         n=8000,
                         include_rts = T,
                         token = token,
                         type = "recent",
                         parse = T,
                         lang = "pt",
                         max_id = "",
                         retryonratelimit = TRUE)
stopwords2 <- as.data.frame(stopwords("portuguese"))


ts_plot(bolsonaro, "mins", color='#08a0e9', size=1) +
  labs(x = NULL, y = NULL,
       title = "Frequência da hashtag #OPovoELegeuBolsonaro",
       subtitle = "05 de agosto de 2020",
       caption = "Fonte: Twitter's REST API")+
  theme_minimal()+
  theme(plot.title = element_text(size=14, face="bold"))

df <- bolsonaro %>%
  select(screen_name, text)

df$texto_limpo <- gsub("http\\S+", "", df$text)

df_1 <- df %>%
  select(texto_limpo) %>%
  unnest_tokens(words, texto_limpo)


clean <- df_1 %>%
  anti_join(stopwords)


names(stopwords) <- "words"
names(stopwords2) <- "words"


stopwords <- read_csv("C:/Arthur/stopwords.txt")
View(stopwords)


clean %>%
  count(words, sort = TRUE) %>%
  filter(n > 200)  %>%
  mutate(word = reorder(words, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


auxCorpus <- Corpus(VectorSource(clean))
auxCorpus <- tm_map(auxCorpus, PlainTextDocument)
auxCorpus <- tm_map(auxCorpus, removePunctuation) 

auxCorpus <- tm_map(auxCorpus, removeWords, stopwords('pt'))
auxCorpus <- tm_map(auxCorpus, stemDocument)
wordcloud(auxCorpus,max.words=50,colors=c("blue","red"))

wordcloud2(data=clean$words, size=1.6, color='random-dark')

dtm <- TermDocumentMatrix(auxCorpus) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

clean %>%
  count(words) %>%
  with(wordcloud(words, n, 
                 colors = brewer.pal(12, "Set1"),
                 max.words = 100)) 

rm(c("df_1", "auxCorpus"))


#### hoje ####
df_corpus <- Corpus(VectorSource(df$text))

## Função que remove HTML 
removeHTML = function(text){
  text = gsub(pattern = '<.+\\">', '', text)
  text = gsub(pattern = '</.+>', '', text)
  return(text)
}

## Removendo HTML, Números, Pontuação, Stopwords e etc...

df_corpus = df_corpus %>%
  tm_map(content_transformer(removeHTML)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("portuguese")) %>%
  tm_map(removeWords, stopwords("SMART")) 

# Criando um Term Document Matrix

tdm = TermDocumentMatrix(df_corpus) %>%
  as.matrix()
words = sort(rowSums(tdm), decreasing = TRUE)
df = data.frame(word = names(words), freq = words)

# ggplot 
library(ggplot2)
install.packages("extrafont")
library(extrafont)

df %>%
  filter(freq>300) %>%
  ggplot(aes(x=reorder(word, -freq),y=freq), fill=freq) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

df %>%
  top_n(n=30, freq) %>%
  ggplot(aes(x=reorder(word, -freq),y=freq)) +
  geom_col(fill="#08a0e9") +
  labs(x = NULL, y = NULL,
       title = "Top 30 palavras da hashtag #OPovoELegeuBolsonaro",
       subtitle = "05 de agosto de 2020",
       caption = "Fonte: Twitter's REST API")+
  theme_minimal()+
  theme(plot.title = element_text(size=14, face="bold"))+
  coord_flip()

wordcloud2(df, size = 1.6, fontFamily = "Segoe UI", fontWeight = "bold",
           color = "random-dark")  

uxc.colors = c("#fefefe", "#f4f2a8", "#030303")
uxc.colors = c("#fefefe", "#030303")

cores_tt = c("#08a0e9", "#EF8F5FD", "0084B4","#fefefe" )
uxc.background = "#00ccff"

library(extrafont)
# font_import()
fonts()

wordcloud2(df,
           color = rep_len(uxc.colors, nrow(df)),
           backgroundColor = uxc.background,
           fontFamily = "DM Sans",
           size = 1.8,
           minSize = 8,
           rotateRatio = 0)

### TF-IDF
tdm2 <- TermDocumentMatrix(df_corpus)
FeatureMatrix <- weightTfIdf(tdm)

bolsonaro %>% 
  filter(!is.na(place_full_name)) %>% 
  count(place_full_name, sort = TRUE) %>% 
  top_n(10)%>%
    ggplot(aes(x=reorder(place_full_name, -n),y=n))+
    geom_col(fill="#08a0e9") +
    labs(x = NULL, y = NULL,
         title = "Top 10 tweets da #OPovoElegeuBolsonaro por localização",
         subtitle = "05 de agosto de 2020",
         caption = "Fonte: Twitter's REST API")+
    theme_minimal()+
    theme(plot.title = element_text(size=14, face="bold"))+
    coord_flip()
  

a <-bolsonaro %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name))

write.csv(a, file = 'arrobas.csv')

c <- bolsonaro %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"),
         hashtag != "#OPovoElegeuBolsonaro") %>%
  count(hashtag, sort = TRUE) %>%
  top_n(10)

  
c %>%
  filter(n<947)%>%
  ggplot(aes(x=reorder(hashtag, -n),y=n)) +
  geom_col(fill="#08a0e9") +
  labs(x = NULL, y = NULL,
       title = "Outras hashtags nos tweets da #OPovoElegeuBolsonaro",
       subtitle = "05 de agosto de 2020",
       caption = "Fonte: Twitter's REST API")+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"))+
  coord_flip()

tm_map(content_transformer(tolower)) %>% #Caixa baixa
  tm_map(removeWords, stopwords("portuguese")) %>% #Removendo stopwords
  tm_map(removeWords, stopwords("SMART"))

#----LIBRARY----
library(tm)
library(Rcpp)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(fmsb)
#data berasal dari tweet berita tentang 'Pfizer-BioNTech' dari kaggle dataset

#----DATA----
#mengeluarkan 400 data agar matriks tidak terlalu besar
data <- pfizer[-c(1:4000),]

#----MEMBENTUK CORPUS----
corpus <- iconv(data$text, to = "UTF-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:4])

#----MEMBERSIHKAN TEKS----
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
pf_vax <- tm_map(corpus, removeWords, stopwords("en"))

removehttps <- function(x) gsub('https[[:alnum:]]*','',x)
special <- function(x) gsub("[^a-zA-Z0-9]", " ", x) #menghilangkan special character

pf_vax <- tm_map(pf_vax, content_transformer(special))
pf_vax <- tm_map(pf_vax, content_transformer(removehttps()))
inspect(pf_vax[1:4])

pf_vax <- tm_map(pf_vax, stripWhitespace)
pf_vax <- tm_map(pf_vax, removeWords, 
                 c('pfizerbiontech','dont','anyone', 'pfizer','says',
                   'just','the','you', 'also','even', 'covid','biontech',
                   'still','can', 'covidvaccine','vaccine','vaccines',
                   'coronavirus','vaccinated','covidvaccination',
                   'vaccination','amp','pfizervaccine'))

#replace kata yang sama
pf_vax <- tm_map(pf_vax, gsub, pattern = 'thanks',
                 replacement = 'thank')
pf_vax <- tm_map(pf_vax, gsub, pattern = 'doses',
                 replacement = 'dose')
pf_vax <- tm_map(pf_vax, gsub, pattern = 'shots',
                 replacement = 'shot')
inspect(cleanbbc[1:4])

#----TERM DOCUMENT MATRIX----
tdm <- TermDocumentMatrix(pf_vax)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

#----VISUALISASI DATA----
#1 Bar Plot
b <- rowSums(tdm)
b <- subset(b, b>=50)

barplot(b,
        las = 2,
        col = rainbow(50))

#2 Wordcloud
word <- sort(rowSums(tdm), decreasing = T)
set.seed(1000)
#versi 1
wordcloud(words = names(word),
          freq = word,
          max.words = 100,
          random.order = F,
          min.freq = 35,
          colors = brewer.pal(5, 'Pastel2'),
          scale = c(5, 0.4),
          rot.per = 0.2)

#versi 2
w <- data.frame(names(b), b)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.8,
           shape = 'circle')

#----SENTIMENT ANALYSIS----
#1 Skor sentimen
tweet <- iconv(pfizer$text, to = "utf-8")
s <- get_nrc_sentiment(tweet)
head(s)

#2 Bar Plot sentimen
barplot(colSums(s),
        las = 2,
        col = rainbow(9),
        density = 60,
        ylab = 'Count',
        main = 'Sentiment Score Pfizer Tweet')

#3 Radar Plot
spider <- as.data.frame(t(colSums(s)))
spider[nrow(spider) + 1,] = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0) #menambah row baru
spider[nrow(spider) + 1,] = c(11000, 11000, 11000, 11000,
                              11000, 11000, 11000, 11000,
                              11000, 11000) #menambah row baru

#rearrange row; urutannya 'max', 'zero, 'min'
spiderweb <- spider %>%
  arrange(factor(anger, levels = c(11000, 0, 1739)))

radarchart(spiderweb, pcol = 'darkblue',
           pfcol = 'lightblue',
           plwd = 1, 
           cglcol = 'navy',
           cglty = 3)

#----END----
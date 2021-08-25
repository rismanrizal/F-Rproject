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
#data berasal dari tweet berita tentang 'kesehatan' dari kantor berita BBC

#----MEMBENTUK CORPUS----
corpus <- iconv(bbc$Tweet, to = "utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:4])

#----MEMBERSIHKAN TEKS----
corpus <- tm_map(corpus, tolower) #lowercase
corpus <- tm_map(corpus, removePunctuation) #menghilangkan tanda baca
corpus <- tm_map(corpus, removeNumbers) #menghilangkan angka
cleanbbc <- tm_map(corpus, removeWords, stopwords("en")) #menghilangkan stopwords

#membuat fungsi untuk menghilangkan alamat web
removehttp <- function(x) gsub('http[[:alnum:]]*','',x)
cleanbbc <- tm_map(cleanbbc, content_transformer(removehttp))

cleanbbc <- tm_map(cleanbbc, stripWhitespace) #menghilangkan spasi
#menghilangkan beberapa kata tidak esensial yang tidak termasuk dalam stopwords
cleanbbc <- tm_map(cleanbbc, removeWords, c('video','audio'))

inspect(cleanbbc[1:4])

#----TERM DOCUMENT MATRIX----
tdm <- TermDocumentMatrix(cleanbbc) 
tdm <- as.matrix(tdm) #membentuk matriks, semakin besar data semakin gigantic matriks
tdm[1:10, 1:20]

#----VISUALISASI DATA----
#1 Bar Plot
word <- rowSums(tdm)
word <- subset(word, word>=35) #filter hanya kata dengan frekuensi >=35

barplot(word,
        las = 2,
        col = rainbow(50))

#2 Wordcloud
wordword <- sort(rowSums(tdm), decreasing = T)
set.seed(777)
wordcloud(words = names(word),
          freq = word,
          max.words = 50, #jumlah maksimum kata dalam 'cloud'
          random.order = F,
          min.freq = 35, #jumlah frekuensi minimum kata
          colors = brewer.pal(5, 'Dark2'),
          scale = c(5, 0.2), #perbandingan antara kata frek terbesar dan terkecil
          rot.per = 0.6) #rotasi kata


#----SENTIMENT ANALYSIS
#1 Skor sentimen
tweet <- iconv(bbc$Tweet, to = "utf-8")
s <- get_nrc_sentiment(tweet)
tail(s)

#2 Bar Plot sentimen
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Score BBC')

#----END----

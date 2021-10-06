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
library(lsa)
library(factoextra)
library(cluster)
#data berasal dari tweet berita tentang 'kesehatan' dari kantor berita Fox (300 tweet teratas)

#----MEMBENTUK CORPUS----
corpus <- iconv(fox$tweet, to = "utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:4])

#----MEMBERSIHKAN TEKS----
corpus <- tm_map(corpus, tolower) #lowercase
corpus <- tm_map(corpus, removePunctuation) #menghilangkan tanda baca
corpus <- tm_map(corpus, removeNumbers) #menghilangkan angka
cleanfox <- tm_map(corpus, removeWords, stopwords("en")) #menghilangkan stopwords

#membuat fungsi untuk menghilangkan alamat web
removehttp <- function(x) gsub('http[[:alnum:]]*','',x)
cleanfox <- tm_map(cleanbbc, content_transformer(removehttp))

cleanfox <- tm_map(cleanbbc, stripWhitespace) #menghilangkan spasi
#menghilangkan beberapa kata tidak esensial yang tidak termasuk dalam stopwords
cleanfox <- tm_map(cleanbbc, removeWords, c('video','audio'))

inspect(cleanfox[1:4])

#----TERM DOCUMENT MATRIX----
tdm <- TermDocumentMatrix(cleanfox) 
tdm <- as.matrix(tdm) #membentuk matriks, semakin besar data semakin gigantic matriks
tdm[1:10, 1:20]

#----TES COSINE----
cosmeas <- cosine(tdm)

#----MENENTUKAN BERAPA JUMLAH K----
#1 jumlah klaster vs total Within Sum of Squares
fviz_nbclust(cosmeas, kmeans, method = "wss")

#2 jumlah klaster vs gap statistik
gap_stat <- clusGap(cosmeas,
                    FUN = kmeans,
                    nstart = 25, #inisiasi konfigurasi 
                    K.max = 10, #max clusters to consider
                    B = 50) #total bootstrapped iterations

fviz_gap_stat(gap_stat)

#----CLUSTERING----
set.seed(1806)

# klastering dengan k = 3
kmean <- kmeans(cosmeas, centers = 3, nstart = 25)
kmean

# plot hasil
fviz_cluster(kmean, data = cosmeas)

kmean$size
kmean$withinss

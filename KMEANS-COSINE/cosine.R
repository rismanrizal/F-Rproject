#----LIBRARY----
library(tm)
library(Rcpp)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(lsa)
library(factoextra)
library(cluster)
#data berasal dari tweet berita tentang 'kesehatan' dari kantor berita Fox (301 tweet teratas)

#----MEMBENTUK CORPUS----
corpus <- iconv(fox$tweet, to = "utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:4])

#----MEMBERSIHKAN TEKS/PREPOCESSING----
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers) 
cleanfox <- tm_map(corpus, removeWords, stopwords("en"))
removehttp <- function(x) gsub('http[[:alnum:]]*','',x)
cleanfox <- tm_map(cleanbbc, content_transformer(removehttp))
cleanfox <- tm_map(cleanbbc, stripWhitespace)
cleanfox <- tm_map(cleanbbc, removeWords, c('video','audio'))
inspect(cleanfox[1:4])

#----TERM DOCUMENT MATRIX----
tdm <- TermDocumentMatrix(cleanfox) 
tdm <- as.matrix(tdm)

#----COSINE MEASURE----
cosmeas <- cosine(tdm)

#----MENENTUKAN BERAPA JUMLAH K DENGAN METODE ELBOW----
fviz_nbclust(cosmeas, kmeans, method = "wss")
# gap_stat <- clusGap(cosmeas, FUN = kmeans,nstart = 25, K.max = 10, B = 50)
# fviz_gap_stat(gap_stat)

#----CLUSTERING----
set.seed(1806)
kmean <- kmeans(cosmeas, centers = 3, nstart = 25)
kmean

# plot hasil
fviz_cluster(kmean, data = cosmeas)

kmean$size
kmean$withinss
# END

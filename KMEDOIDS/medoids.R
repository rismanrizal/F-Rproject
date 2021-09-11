#----NOTES----
# contoh pengerjaan clustering dengan k-medoids.
# hanya untuk menunjukkan syntax saja, secara data mungkin ini lebih cocok
# menggunakan k-means clustering

#----CONTOH 1----

#----LIBRARY----
library(factoextra)
library(cluster)
library(readr)
library(tidyverse)

#----TUJUAN----
# ingin mengelempokkan artis berdasarkan beberapa parameter lagu
# data diambil dari spotify top50

# menjadikan kolom 'artist' sebagai rownames
spot <- read.csv('link')
spoti <- spot %>% remove_rownames %>% column_to_rownames(var="artist")

# normalisasi data sehinga memiliki mean 0 dan sd 1
spoti <- scale(spoti)

#----MENENTUKAN BERAPA JUMLAH K----
#1 jumlah klaster vs total Within Sum of Squares
fviz_nbclust(spoti, pam, method = "wss")
# menentukan jumlah k dilihat dari axis dengan 'bending' atau patahan yg paling nampak

#2 jumlah klaster vs gap statistik
gap_stat <- clusGap(spoti,
                    FUN = pam,
                    K.max = 10, #max clusters to consider
                    B = 50) #total bootstrapped iterations
fviz_gap_stat(gap_stat)
# menentukan jumlah k dilihat dari axis yang paling jangkung
# dari #1 dan #2 dapat dilihat bahwa k = 6

#----CLUSTERING----
set.seed(1806)

# klasterisasi dengan k = 6
kmed <- pam(spoti, k = 6)
kmed

# membuat plot
fviz_cluster(kmed, data = spoti)

#----END CONTOH 1----

#----CONTOH 2----

# ingin mengelempokkan klub bbva berdasarkan beberapa statistik pertandingan
bbva <- read.csv('link')
laliga <- bbva %>% remove_rownames %>% column_to_rownames(var="CLUB")
laliga <- scale(laliga)
fviz_nbclust(laliga, pam, method = "wss")
gap_stat <- clusGap(laliga,
                    FUN = pam,
                    K.max = 10, #max clusters to consider
                    B = 50) #total bootstrapped iterations
fviz_gap_stat(gap_stat)
set.seed(1806)
kmed <- pam(laliga, k = 6)
kmed
fviz_cluster(kmed, data = laliga)

#----END CONTOH 2----
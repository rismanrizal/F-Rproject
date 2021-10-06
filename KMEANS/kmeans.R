#----NOTES----
# contoh pengerjaan clustering dengan k-means
# hanya untuk menunjukkan syntax saja

#----LIBRARY----
library(factoextra)
library(cluster)
library(readr)
library(tidyverse)

#----CONTOH 1----
#----TUJUAN----
# ingin mengelempokkan club di eropa berdasarkan beberapa parameter

euclub <- read.csv('link')
club <- euclub %>% remove_rownames %>% column_to_rownames(var="team")

# normalisasi data sehinga memiliki mean 0 dan sd 1
club <- scale(club)

#----MENENTUKAN BERAPA JUMLAH K----
#1 jumlah klaster vs total Within Sum of Squares
fviz_nbclust(club, kmeans, method = "wss")

#2 jumlah klaster vs gap statistik
gap_stat <- clusGap(club,
                    FUN = kmeans,
                    nstart = 25, #inisiasi konfigurasi 
                    K.max = 10, #max clusters to consider
                    B = 50) #total bootstrapped iterations

fviz_gap_stat(gap_stat)

#----CLUSTERING----
set.seed(99)

# klastering dengan k = 3
kmean <- kmeans(club, centers = 3, nstart = 25)
kmean

# plot hasil
fviz_cluster(kmean, data = club)

# menemukan rata-rata dari setiap klaster
aggregate(club, by = list(cluster = kmean$cluster), mean)

# membuat tabel sehingga terlihat club ada di klaster mana
finalclub <- cbind(club, cluster = kmean$cluster)

#----END----

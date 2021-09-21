# RANDOM FOREST DENGAN BANYAK VARIABEL INDEPENDENT
# Tujuan ingin memprediksi klasifikasi jenis ecoli berdasarkan beberapa parameter

#----LIBRARY----
library(randomForest)
library(caret)
library(lattice)
library(ggplot2)
library(yardstick)

#----DATA----
str(ecoli)
sum(is.na(ecoli)) #total jumlah pengamatan yang missing
table(ecoli$class)

#----MEMBAGI DATA----
set.seed(1234)
idx <- sample(2, nrow(ecoli), replace = T, prob = c(0.8,0.2))
trnecoli <- ecoli[idx==1,]
tesecoli <- ecoli[idx==2,]

#----MEMBUAT MODEL RANDOM FOREST----
modelecoli <- randomForest(class~mcg+gvh+lip+chg+aac+
                             alm1+alm2, data = trnecoli, proximity = T)
print(modelecoli)

#----PREDIKSI DAN EVALUASI----

#1 memprediksi model dan confusion matrix - TRAIN DATA
pecoli1 <- predict(modelecoli, trnecoli)
confusionMatrix(pecoli1, trnecoli$class)

#2 memprediksi model dan confusion matriks - TEST DATA
pecoli2 <- predict(modelecoli, tesecoli)
confusionMatrix(pecoli2, tesecoli$class)

#3 error rate untuk mengetahui jumlah optimal trees
plot(modelecoli)

#----VISUAL GRAFIK----
#1 number of nodes in tree
hist(treesize(modelecoli), main = "Jumlah Nodes dalam Tree",
     col = "pink")

#2 variabel importance
varImpPlot(modelecoli, sort = T, n.var = 7) #nvar untuk berapa banyak variabel, misal top 10
varUsed(modelecoli) #menunjukkan variabel mana yang paling dominan digunakan dalam model

#3 partial dependence plot
partialPlot(modelecoli, trnecoli, alm1, "cp")
# if the alm1 is less than the lowest point of line chart,
# then higher chances of classifying into "Setosa class "cp" class

#4 extract single tree
getTree(modelecoli, 1, labelVar = T)

#5 multidimensional plot of proximity matriks
MDSplot(modelecoli, tesecoli$class)

#6 misslassification matrix
confusionMatrix(pecoli2, tesecoli$class)

#7 membuat visualisasi heatmap confusion matrix
#7.1 membuat list nilai prediksi menjadi dataframe
predDF <- data.frame(matrix(unlist(pecoli2), nrow=61,
                            byrow=TRUE),stringsAsFactors=FALSE)

colnames(predDF) <- "prediksi" # mengubah nama kolom dari nilai prediksi tsb
completetest <- cbind(tesecoli, predDF) # menggabungkan dataframe
str(completetest)
completetest$prediksi <- as.factor(completetest$prediksi)

#7.2 membuat heatmap
cm <- conf_mat(completetest, class, prediksi)
# pada bagian ini, jumlah kelas faktor tidak sama, sehingga tidak dapat dibuat heatmap
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="white",high = "blue") + 
  theme(legend.position = "right")

#----END----
# SIMPLE RANDOM FOREST
# Tujuan ingin memprediksi klasifikasi orang dalam melakukan pembelian dari iklan

#----LIBRARY----
library(randomForest)
library(caret)
library(lattice)
library(ggplot2)
library(yardstick)

#----DATA----
str(ads)
# perlu dicek apakah ada missing value

sum(!complete.cases(ads))
# apabila ternyata ada, dapat digantikan dengan median/mean menggunakan formula
# for(i in 1:ncol(ads)){ads[ , i][is.na(ads[ , i])] <- median(ads[ , i], na.rm=TRUE)}

# mengubah variabel y dan sex menjadi faktor
ads$beli <- as.factor(ads$beli)
ads$sex <- as.factor(ads$sex)

# mengecek proporsi beli
table(ads$beli)

#----MEMBAGI DATA----
set.seed(1988)
index <- sample(2, nrow(ads), replace = T, prob = c(0.75,0.25))
train <- ads[index==1,]
test <- ads[index==2,]

#----MODEL RANDOM FOREST----
model <- randomForest(beli~sex+usia+gaji, data = train)
print(model)

#----PREDIKSI DAN EVALUASI MODEL----

#1 memprediksi model dan confusion matrix dari Train Data
p1 <- predict(model, train)
confusionMatrix(p1, train$beli)

#2 memprediksi model dan confusion matriks dari Test Data
p2 <- predict(model, test)
confusionMatrix(p2, test$beli)

#3 error rate untuk mengetahui jumlah optimal trees
plot(model)

#----TUNING MODEL (APABILA DIBUTUHKAN)----
#1 tune menggunakan algoritma
xvar <- as.data.frame(cbind(train$sex, train$usia, train$gaji))
tu <- tuneRF(x = xvar, y = train$beli,
       stepFactor = 0.5,
       plot = T, ntreeTry = 201, trace = T, 
       improve = 0.05)

#2 tune menggunakan Random Search
control <- trainControl(method="repeatedcv", number=10,
                        repeats=3, search="random")
set.seed(1988)
mtry <- sqrt(ncol(xvar))
rf_random <- train(beli~sex+usia+gaji, data=train, method="rf",
                   metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

# dari 1 dan 2 diperoleh mtry terbaik = 2


#----MODEL RANDOM FOREST ARTER TUNING----
# awal OOB 10,69%
model2 <- randomForest(beli~sex+usia+gaji, data = train,
                       ntree = 501, mtry = 2, importance = T,
                       proximity = T)
print(model2)
# OOB turun sedikit menjadi 10,00%

#----JUMLAH NODES PADA TREE----
hist(treesize(model2), main = "Jumlah Nodes dalam Tree",
     col = "brown")

#----VISUAL GRAFIK----
#1 variabel importance
varImpPlot(model2, sort = T, n.var = 3) #nvar untuk berapa banyak variabel, misal top 10
varUsed(model) #menunjukkan variabel mana yang paling dominan digunakan dalam model

#2 partial dependence plot
partialPlot(model2, train, usia, "1")

#2 extract single tree
getTree(model2, 1, labelVar = T)

#4 multidimensional plot of proximity matriks
MDSplot(model2, train$beli) 

#5 misslassification matrix
confusionMatrix(p2, test$beli)

#6 membuat visualisasi heatmap confusion matrix
#6.1 membuat list nilai prediksi menjadi dataframe
predDF <- data.frame(matrix(unlist(p2), nrow=110,
                            byrow=TRUE),stringsAsFactors=FALSE)

colnames(predDF) <- "prediksi" # mengubah nama kolom dari nilai prediksi tsb
completetest <- cbind(test, predDF) # menggabungkan dataframe

#6.2 membuat heatmap
cm <- conf_mat(completetest, beli, prediksi)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="white",high = "blue") + 
  theme(legend.position = "right")

#----END----
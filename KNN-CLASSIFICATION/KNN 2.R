#----KNN CLASSIFICATION----
library(caret)
library(pROC)
library(InformationValue)

#----DATASET----
#dataset menggunakan 'haberman' dari UCI Machine Learning Repository
#memberikan nama kolom
mydata <- setNames(mydata, c("ageoperat","age", "nodes", "survival"))

#karakteristik, cek missing value, dan menjadikan survival kolom sebagai faktor
str(mydata)
sum(is.na(mydata))
mydata$survival[mydata$survival == 1] <- 'Died'
mydata$survival[mydata$survival == 2] <- 'Live'
mydata$survival <- factor(mydata$survival)

#proporsi survival
table(mydata$survival)

#----MEMBAGI DUA KELAS: TRAINING & TESTING----
set.seed(1806)
index <- sample(1:nrow(mydata), 0.7*nrow(mydata))
train <- mydata[index,]
test <- mydata[-index,]

#----MEMBUAT MODEL KNN----
#cross validation
kontrol <- trainControl(method = "repeatedcv",
                        number = 10, repeats = 5)

set.seed(1988)
modelfit <- train(survival~., data = train,
                  method = 'knn',
                  tuneLength = 20,
                  trControl = kontrol,
                  preProc = c("center", "scale"))
#preProc berguna untuk menstandardisasi variabel 
#model ini akan menghasilkan optimal model dengan parameter Accuracy
#bisa juga optimal model dengan parameter ROC
#metric = ROC pada modelnya

#----EVALUASI MODEL----
modelfit
plot(modelfit)
varImp(modelfit)

#----PREDIKSI MODEL----
prediksi <- predict(modelfit, newdata = test)
confusionMatrix(prediksi, test$survival)

#----END----
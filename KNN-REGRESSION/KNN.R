#----KNN REGRESSION----
library(caret)
library(pROC)

#----DATASET----
#dataset menggunakan 'abalone' dari UCI Machine Learning Repository
#memberikan nama kolom
abalone <- setNames(abalone, c("sex", "length", "diameter",
                         "height", "w_weight", "s_weight",
                         "v_weight", "shell", "rings"))

#karakteristik dan menjadikan sex kolom sebagai faktor
str(abalone)
abalone$sex <- as.factor(abalone$sex)
summary(abalone)

#mengevaluasi apakah terdapat missing data
sum(is.na(abalone))

#----MEMBAGI DUA KELAS: TRAINING & TESTING----
set.seed(1806)
ind <- sample(1:nrow(abalone), 0.8*nrow(abalone))
training <- abalone[index,]
testing <- abalone[-index,]

#----MEMBUAT MODEL KNN----
#cross validation
kontrol <- trainControl(method = 'repeatedCV', number = 10,
                          repeats = 3)
set.seed(1988)
modelfitabalone <- train(rings~., data = training,
                         tuneGrid = expand.grid(k=1:50),
                         method = 'knn', trControl = kontrol,
                         preProc = c('center', 'scale'))
#preProc berguna untuk menstandardisasi variabel 

#----EVALUASI MODEL----
modelfitabalone
plot(modelfitabalone)
varImp(modelfitabalone)
#model ini akan menghasilkan optimal model dengan parameter RMSE
#bisa juga optimal model dengan parameter rsquared
#metric = rsquared pada modelnya

#----PREDIKSI MODEL----
prediksiabalone <- predict(modelfitabalone, newdata = testing)
RMSE(prediksiabalone, testing$rings)
plot(prediksiabalone ~ testing$rings)

#----END----
# SVM DENGAN E1071 TANPA MEMBAGI DATA TRAIN & TEST

#----LIBRARY----
library(ggplot2)
library(e1071)

#----CONTOH I: DUA KELAS----

#----Data----
# data yang digunakan adalah Banknote Classification dari UCI
data$class <- as.factor(data$class)
str(data)
# membentuk scatter plot
# memberikan warna berdasarkan class sehingga terlihat ada klasifikasi
qplot(var, curt, data = data, color = class)

#----Model----
model <- svm(class~., data = data)
# kernel data diubah (radial-default, menjadi linier, polynomial, sigmoid
# kernel adalah batas vektor

summary(model)
plot(model, data = data, var~skew)
# dapat juga diatur 'slice' atau x-y berdasarkan remaining variabel secara konstan

#----Evaluasi Model----
# prediksi dan confusion matrix
prediksi <- predict(model, data)
tabel <- table(Predicted = prediksi, Actual = data$class)
tabel

# misclassification error
1-sum(diag(tabel))/sum(tabel)

# tidak butuh tuning
#----End----


#----CONTOH II: MULTICLASS----
#----Data----
# data yang digunakan adalah Stars dari Kaggle
# perlu distandardisasi karena datanya cukup heterogen
stars$type <- as.factor(stars$type)
stars$temp <- scale(stars$temp)
stars$L <- scale(stars$L)
stars$R <- scale(stars$R)
stars$A_M <- scale(stars$A_M)
stars$color <- as.factor(stars$color)
stars$spectral <- as.factor(stars$spectral)
str(stars)

qplot(R, L, data = stars, color = type)

#----Model----
modelstars <- svm(type~., data = stars)
summary(modelstars)
plot(modelstars, data = stars, temp~A_M)

#----Evaluasi Model----
pred <- predict(modelstars, stars)
tab <- table(Predicted = pred, Actual = stars$type)
tab
1-sum(diag(tab))/sum(tab)
# tidak butuh tuning
#----End----

#----CONTOH III: MULTICLASS----
#----Data----
# data yang digunakan adalah Abalone dari UCI
abalone$sex <- as.factor(abalone$sex)
str(abalone)
qplot(diameter, height, data = abalone, color = sex)

#----Model----
modelabalone <- svm(sex~., data = abalone)
summary(modelabalone)
plot(modelabalone, data = abalone, height~shell)

#----Evaluasi Model----
predaba <- predict(modelabalone, abalone)
tabelaba <- table(Predicted = predaba, Actual = abalone$sex)
tabelaba
1-sum(diag(tabelaba))/sum(tabelaba)

# ujicoba kernel
modelabalone1 <- svm(sex~., data = abalone,
                     kernel = "sigmoid") #dan 'linear', 'polynomial'
summary(modelabalone1)
plot(modelabalone1, data = abalone, height~shell)

# confusion matrix dan misclassification error
predaba1 <- predict(modelabalone1, abalone)
tabelaba1 <- table(Predicted = predaba1, Actual = abalone$sex)
tabelaba1
1-sum(diag(tabelaba1))/sum(tabelaba1)
# ujicoba untuk kernel linear, polynomial, dan sigmoid
# menghasilkan missclasifikasi yang makin tinggi; kernel=radial terbaik

#----Tuning Model----
set.seed(1806)
tunemodel <- tune(svm, sex~., data = abalone,
                  ranges = list(epsilon = seq(0,1,0.2), #seq dari 0 s.d. 1 dengan kenaikan 0.2
                                cost = 2^(4:6))) #dua pangkat 4 s.d. pangkat 6
plot(tunemodel) 
summary(tunemodel)

# memilih model terbaik
updmodel <- tunemodel$best.model
summary(updmodel)
plot(updmodel, data = abalone, height~shell)

# confusion matrix dan misclassification error
predaba2 <- predict(updmodel, abalone)
tabelaba2 <- table(Predicted = predaba2, Actual = abalone$sex)
tabelaba2
1-sum(diag(tabelaba2))/sum(tabelaba2)
# setelah tuning, ada kenaikan ketepatan prediksi dan penurunan missclassifiaction error

#----END----

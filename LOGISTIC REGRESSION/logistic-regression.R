#----LIBRARY----
library(caret)
library(ggplot2)
library(lattice)
library(InformationValue)
library(pscl)

#----KARAKTERISTIK DATA----
#fungsi untuk melihat cek apakah ada missing value
sum(!complete.cases(data))
str(data)

#mengubah DV menjadi factor
data$ca <- ifelse(test = data$ca == 1, yes = "Yes", no = "No")
data$ca <- as.factor(data$ca)
data$rm <- as.factor(data$rm)
data$ac <- as.factor(data$ac)
data$ia <- as.factor(data$ia)
data$tlod <- as.factor(data$tlod)
data$ir <- as.factor(data$ir)
data$gc <- as.factor(data$gc)
data$ea <- as.factor(data$ea)
data$reg <- as.factor(data$reg)
data$exc <- as.factor(data$exc)

#melihat proporsi DV dan IV
table(data$ca)
table(data$ca)
table(data$tlod)
table(data$ir)
table(data$gc)
table(data$reg)
summary(data) 
#ternyata adanya class imbalance pada 'rm', 'ac', 'ia', 'ea'

#----MEMBUAT DUA KELAS: TRAIN dan TEST----
set.seed(135)
#asumsikan 65% data untuk train sisanya untuk test
index <- sample(1:nrow(data), 0.65*nrow(data))
train <- data[index,]
test <- data[-index,]

#mengecek class balancing DV 'ca' pada train dan test data
table(train$ca)
table(test$ca)

#terdapat absolute class unbalance pada 'ea', ea dikeluarkan
#terdapat extreme class unbalance pada 'rm', 'ac', 'ia', mereka dikeluarkan
#terdapat korelasi sempurna antara 'reg' dan 'exc', exc dikeluarkan


#----MEMBUAT MODEL LOGISTIK----

mymodel <- glm(ca~price+marketcap+tlod+ir+gc+reg, 
          family = "binomial", data = train)

mymodel
summary(mymodel)


#----EVALUASI MODEL FIT----
library(rcompanion)
#1 pseudo R2
nagelkerke(mymodel)

#2 Hosmer Lemeshow model fit test
library(generalhoslem)
Hosmer <- logitgof(train$ca, fitted(mymodel))
Hosmer

#3 melihat variabel importance + grafiknya
varImp(mymodel)
VI <- varImp(mymodel)
ggplot(VI, aes(x = reorder (rownames(VI), Overall), y = Overall))+
  geom_point(color = "steelblue", size = 5)+
  xlab('Variabel') + ylab('Overall Importance') + coord_flip()

#4 melihat multikolinearitas dengan VIF
library(blorr)
blr_vif_tol(mymodel)


#----MEMBUAT PREDIKSI----
#probability dari setiap perusahaan 
pred <- predict(mymodel, test, type="response")
head(pred)
head(test)

ujicoba <- data.frame(price = 3.6, marketcap = 23,
                      tlod = '1', ir = '1', gc = '0', reg = '1')
predict(LR, ujicoba, type="response")


#----MODEL PREDICTION ACCURACY----
#pada binary, cut off probability adalah 0.5
#pada beberapa model, bisa berbeda
#perlu ditentukan optimal prediction probability cut off 
#convert 'ca' from "Yes" and "No" to 1's and 0's
test$ca <- ifelse(test$ca == "Yes", 1, 0)
optimalCO <- optimalCutoff(test$ca, pred)
optimalCO

#membuat confussion matrix
confusionMatrix(test$ca, pred, threshold = optimalCO)
table(ActualValue = test$ca, PredictedValue=pred>optimalCO)

#visualisasi confusion matriks
library(dplyr)
#membuat tabel dari confusion matriks
tabelCM <- matrix(c("No","No", 26,
                    "No", "Yes", 2,
                    "Yes", "No", 2,
                    "Yes", "Yes", 16), 
                  ncol = 3, byrow = T)
colnames(tabelCM) <- c("Aktual", "Prediksi", "n")
rownames(tabelCM) <- c("TN", "FN", "FP", "TP")

library(cvms)
library(tibble)
CnM <- as_tibble(tabelCM)
plot_confusion_matrix(CnM,  target_col = "Aktual", 
                      prediction_col = "Prediksi",
                      counts_col = "n", add_normalized = F,
                      palette = "Greens")

#hitung sensitivity (atau “true positive rate”: ketepatan prediksi 'Yes')
sensitivity(test$ca, pred, threshold = optimalCO)

#dan specificity (atau “true negative rate”: ketepatan prediksi 'No')
specificity(test$ca, pred, threshold = optimalCO)

#derajat kekeliruan dalam melakukan klasifikasi
misClassError(test$ca, pred, threshold=optimalCO)

#akurasi, presisi
akurasi <- ((26+16)/(26+2+16+2))
presisi <- 26/(26+2)
akurasi
presisi

#----ROC CURVE----
plotROC(test$ca, pred)

#probability plot
predicted.data <- data.frame(mymodel$fitted.values, data$ca)
predicted.data <- predicted.data[
  order(predicted.data$mymodel.fitted.values, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)
ggplot(data=predicted.data, aes(x=rank, y = mymodel$fitted.values)) +
  geom_point(aes(color=data$ca), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of CA implementation")

#-----END----
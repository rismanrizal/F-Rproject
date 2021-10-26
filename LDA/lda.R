#----LIBRARY----
library(psych) #for pairs
library(heplots) #for boxM
library(rstatix) #for box_m
library(MASS) #for lda
library(corrplot)
library(devtools)
install_github("fawda123/ggord")
library(ggord)
library(klaR)

#----DATA----
## data diambil dari Vertebral Data UCI
str(data)
data$class <- as.factor(data$class)
table(data$class)

#----VISUALISASI DATA----
pairs.panels(data[1:4], gap = 0.5,
             bg = c("red","yellow","blue")[data$class],
             pch = 22)

corr.matrix <- cor(data[1:6])
corr.matrix
corrplot.mixed(corr.matrix)

#----TES ASUMSI----
## lda memerlukan asumsi multivariate normality
sum(!complete.cases(data))
group <- as.vector(data$class) #membentuk vector variabel target
boxM(data[-7], group) #metode 1
box_m(data[,1:6], group) #metode 2
str(data)

#----MEMBAGI DATA----
set.seed(1988)
index <- sample(2, nrow(data), replace = T, prob = c(0.70,0.30))
train <- data[index==1,]
test <- data[index==2,]

#----MEMBENTUK MODEL----
model <- lda(class~., data = train)
model

plot(model)

#----VISUALISASI MODEL----
## Histogram
prediksi <- predict(model, train)
prediksi

ldahist(data = prediksi$x[,1], g = train$class)
ldahist(data = prediksi$x[,2], g = train$class)

## Biplot
ggord(model, train$class, ylim = c(-15,15))

## Partition plot
partimat(class~., data = train, method = "lda")

#----EVALUASI DAN AKURASI----
## akurasi training data
prediktrain <- predict(model, train)$class
tabeltrain <- table(Predicted = prediktrain, Actual = train$class)
tabeltrain
sum(diag(tabeltrain))/sum(tabeltrain)

## akurasi testing data
prediktest <- predict(model, test)$class
tabeltest <- table(Predicted = prediktest, Actual = test$class)
tabeltest
sum(diag(tabeltest))/sum(tabeltest)

## akurasi model dapat dibuat juga sebagai berikut
mean(prediktest == test$class)

#----END----

#----LIBRARY----
library(ggplot2)
library(dplyr)
library(psych)
library(naivebayes)

#----DATA----
#data menggunakan 'haberman' from UCI Dataset
table(haberman$status) ##minimal 5 observasi per kelas
str(haberman)
haberman$status <- as.factor(haberman$status)

#----VISUALISASI DATA----
#1 Bebas Korelasi: antar variabel harus bebas dari korelasi kuat
pairs.panels(haberman[-4])

#2 Box Plot: untuk ketiga variabel
haberman %>%
  ggplot(aes(x = status, y = age, fill = status))+
  geom_boxplot()+
  ggtitle("Box Plot Age")

#3 Density Plot
haberman %>%
  ggplot(aes(x = age, fill = status))+
  geom_density(alpha = 0.5, color = 'yellow')+
  ggtitle("Density Plot Age")

#----SPLIT THE DATA----
set.seed(3108)
index <- sample(1:nrow(haberman), 0.7*nrow(haberman))
train <- haberman[index,]
test <- haberman[-index,]

#----THE MODEL----
mymodel <- naive_bayes(status ~ ., data = train)
mymodel
plot(mymodel)

#----PREDIKSI----
prediksi <- predict(mymodel, train, type = 'prob')
tail(cbind(prediksi, train))

#1 Confusion matriks training
pred1 <- predict(mymodel, train)
(tab1 <- table(pred1, train$status))
1-sum(diag(tab1) / sum(tab1))

#2 Confusion matriks testing
pred2 <- predict(mymodel, test)
(tab2 <- table(pred2, test$status))
1-sum(diag(tab2) / sum(tab2))

#----END----
# NOTES: MODEL INI BUKAN MERUPAKAN MODEL TERBAIK!

#----LIBRARY----
library(party)
library(rpart)
library(rpart.plot)
library(caret)
library(yardstick)
library(ggplot2)

#----DATA----
# data diambil dari UCI dataset tentang pemilihan alat kontrasepsi di Indonesia
# mengubah target variabel menjadi factor
which(!complete.cases(data))
str(data)
data$c_method_factor <- as.factor(data$c_method)

#----MEMBAGI DATA----
set.seed(1806)
index <- sample(1:nrow(data), 0.8*nrow(data))
train <- data[index,]
test <- data[-index,]

#----DENGAN LIBRARY PARTY----
#----MODEL TREE----
# model tree basic
tree_party <- ctree(c_method_factor ~ age + education + child + 
                      religion + working + living, data = train)
tree_party
plot(tree_party)

# mencoba untuk 'prune' the tree mencari model terbaik dengan beberapa kontrol
tree_party2 <- ctree(c_method_factor ~ age + education + child + 
                      religion + working + living, data = train,
                     controls = ctree_control(mincriterion = 0.95, 
                                              minsplit = 200))
# mincriteria adalah significance
# minsplit adalah branch akan terbagi apabila minimum sampel 200 > more compact
plot(tree_party2)

#----PREDIKSI MODEL----
predict(tree_party2, test, type = 'prob') #untuk menampilkan probability
predict(tree_party2, test) #untuk direct menampilkan klasifikasi

#----MISCLASSIFICATION ERROR----
#1 untuk Train data
misclass_train <- table(predict(tree_party), train$c_method_factor)
print(misclass_train)
1-sum(diag(misclass_train))/sum(misclass_train)

#2 untuk Test data
pred_test <- predict(tree_party, newdata = test)
misclass_test <- table(pred_test, test$c_method_factor)
print(misclass_test)
1-sum(diag(misclass_test))/sum(misclass_test)

#----DENGAN LIBRARY RPART----
tree_rpart <- rpart(c_method_factor ~ age + education + child + 
                     religion + working + living, data = train)
rpart.plot(tree_rpart, extra = 1)

#----PREDIKSI MODEL----
pred_rpart <- predict(tree_rpart, test, type = 'class') #untuk direct menampilkan klasifikasi

#----MISCLASSIFICATION ERROR----
# dengan menggunakan confusion matriks
confusionMatrix(pred_rpart, test$c_method_factor)

# membuat visualisasi heatmap confusion matrix
# membuat list nilai prediksi menjadi dataframe
predDF <- data.frame(matrix(unlist(pred_rpart), nrow=295,
                        byrow=TRUE),stringsAsFactors=FALSE)

colnames(predDF) <- "pred" # mengubah nama kolom dari nilai prediksi tsb
completetest <- cbind(test, predDF) # menggabungkan dataframe

# heatmap
cm <- conf_mat(completetest, c_method_factor, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="white",high = "orange") + 
  theme(legend.position = "right")

#----END----


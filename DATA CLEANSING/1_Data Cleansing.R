#----MENDETEKSI NA VARIABEL DAN OUTLIER----
#----LIBRARY----
library(dplyr)

#----DATASET----
#load data yang akan digunakan (credit risk) 32.581 row

str(cr) #melihat spesifikasi data
head (cr) #melihat data awal

#menemukan cell dengan nilai na/kosong
which(complete.cases(cr)) #cell komplit 
which(!complete.cases(cr)) #cell na 

#membuat dataframe baru dengan menghilangkan nilai na
cr_noNA <- which(!complete.cases(cr))
crv1 <- cr[-cr_noNA,] #diperoleh 28.638 row

#membuat histogram dari data
hist(crv1$experience) #menghasilkan kecenderungan skewed

#melihat outlier dengan Boxplot
#karena boxplot, maka perlu melihat per masing-masing variabel double
boxplot(crv1$age, horizontal = T)
quantile(crv1$age) #dugaan di atas 45 menjadi outlier
boxplot(crv1$income, horizontal = T)
quantile(crv1$income) #dugaan di atas 800.000
boxplot(crv1$experience, horizontal = T)
quantile(crv1$experience) #dugaan di atas 18
boxplot(crv1$loan_amount, horizontal = T)
quantile(crv1$loan_amount) #dugaan di atas 24.000
boxplot(crv1$rate, horizontal = T)
quantile(crv1$rate) #dugaan di atas 22

#menghilangkan outlier bersamaan
crv2 <- subset(crv1, crv1$age<45 & crv1$income<800000 &
                 crv1$experience<18 & crv1$loan_amount<24000 &
                 crv1$rate<22)
dim(crv2)

#mengecek ulang outlier kedua
boxplot(crv2$age, horizontal = T) #dugaan di atas 40 menjadi outlier
boxplot(crv2$income, horizontal = T) #dugaan di atas 175.000
boxplot(crv2$experience, horizontal = T) #dugaan di atas 14
boxplot(crv2$loan_amount, horizontal = T) #dugaan di atas 22.500
boxplot(crv2$rate, horizontal = T) #dugaan di atas 21

#menghilangkan outlier bersamaan kedua
crv3 <- subset(crv2, crv2$age<40 & crv2$income<175000 &
                 crv2$experience<14 & crv2$loan_amount<22500 &
                 crv2$rate<21)
dim(crv3)

#mengecek ulang outlier ketiga
boxplot(crv3$age, horizontal = T) #OK
boxplot(crv3$income, horizontal = T) #dugaan di atas 130.000
boxplot(crv3$experience, horizontal = T) #OK
boxplot(crv3$loan_amount, horizontal = T) #OK
boxplot(crv3$rate, horizontal = T) #OK

#menghilangkan outlier bersamaan ketiga
crv4 <- subset(crv3, crv3$income<130000)
dim(crv4)

#menghilangkan outlier bersamaan keempat
boxplot(crv4$income, horizontal = T) #dugaan di atas 121.000

#menghilangkan outlier bersamaan ketiga
crv5 <- subset(crv4, crv4$income<121000)
dim(crv5)

#menghilangkan outlier bersamaan kelima
boxplot(crv5$income, horizontal = T) #OK

#melihat data
head(crv5)

#menyimpan data hasil seleksi ke csv
write.csv(crv5, file = "crclean.csv", row.names = T)

#----END----
#==== Library dan Data ====

pacman::p_load(dplyr, ggplot2, lubridate, ggthemes, remotes,
               neuralnet, dummy, Metrics, tidyverse, readxl,
               tictoc, knitr, kableExtra)

# install_github("bearloga/maltese") <- install terlebih dahulu
library(maltese)

# data
mydata <- read_excel("export data.xlsx")

# visualisasi time series
mydata %>% 
  ggplot(aes(x = date, y = export_growth)) +
  geom_line(color = "chocolate2") + 
  geom_point(color = "chocolate4") +
  theme_solarized() +
  labs(
    title = 'Export Growth',
    subtitle = 'Period Feb-2016 until Feb-2022 (%)') +
  xlab("Growth") + ylab("Date")


#==== Splitting & Preprocessing ====

splitdate <- "2021-03-01"
table(ifelse(mydata$date < splitdate, "training", "testing"))

# normalisasi data
normal_ <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, 
  args = list(x = mydata$export_growth[mydata$date < splitdate]))

mydata$normalized <- (mydata$export_growth - normal_$mean)/normal_$std.dev
mydata_df <- as.data.frame(mydata)

# new features as input
# misal digunakan 5 observasi sebelumnya (p = 5)
mlts_data <- mlts_transform(mydata_df, date, normalized, 
                            p = 5, extras = T, extrasAsFactors = T,
                            granularity = "month")

mlts_data$mlts_extras_month <- month(mlts_data$dt, label = T)

# convert factor ke dalam bentuk dummy
mlts_cat <- categories(mlts_data[, "mlts_extras_month", drop = F])
mlts_dum <- cbind(mlts_data, dummy(mlts_data[, "mlts_extras_month",
                                             drop = F], 
                                   object = mlts_cat, int = T))

str(mlts_dum, list.len = 15)


#==== Training Model ====

# training model
training <- which(mlts_dum$dt < splitdate)
testing <- which(mlts_dum$dt >= splitdate)

# membentuk formula
nn_fitur <- grep("(mlts_lag_[0-9])|(mlts_extras_month_.*)",
                 names(mlts_dum), value = T)
nn_form <- as.formula(paste("y ~ ", paste(nn_fitur, collapse = " + ")))


## model
set.seed(2044)
nn_model1 <- neuralnet(nn_form, mlts_dum[training, c("y", nn_fitur)],
                       linear.output = T, hidden = c(5,3),
                       algorithm = "backprop", learningrate = 0.01,
                       stepmax = 1e7)

nn_model2 <- neuralnet(nn_form, mlts_dum[training, c("y", nn_fitur)],
                       linear.output = T, hidden = c(7,5,3),
                       algorithm = "backprop", learningrate = 0.01,
                       stepmax = 1e7)

nn_model3 <- neuralnet(nn_form, mlts_dum[training, c("y", nn_fitur)],
                       linear.output = T, hidden = c(7,5,5,3),
                       algorithm = "backprop", learningrate = 0.01,
                       stepmax = 1e7)

nn_model4 <- neuralnet(nn_form, mlts_dum[training, c("y", nn_fitur)],
                       linear.output = T, hidden = c(7,7,5,3),
                       algorithm = "backprop", learningrate = 0.01,
                       stepmax = 1e7)


#==== Evaluasi Model ====

error1 <- nn_model1$result.matrix[1,1]
error2 <- nn_model2$result.matrix[1,1]
error3 <- nn_model3$result.matrix[1,1]
error4 <- nn_model4$result.matrix[1,1]

error_nn <- data.frame(Model = (c("Model 1", "Model 2", "Model 3", "Model 4")),
                       Error = round(c(error1, error2, error3, error4),
                                     digits = 8))

error_nn %>%
  kbl() %>%
  kable_paper(full_width = T) %>%
  column_spec(2, color = "white",
              background = spec_color(1:4, end = 0.4, option = "D", direction = -1),
              popover = paste("am:", error_nn$am[1:4]))


# nilai prediksi
nn_pred3 <- as.numeric(compute(nn_model3,
                               mlts_dum[testing, nn_fitur])$net.result)
nn_pred4 <- as.numeric(compute(nn_model4, 
                               mlts_dum[testing, nn_fitur])$net.result)

# denormalisasi
prediksi3 <- data.frame(
  date = mlts_dum$dt[testing],
  normalized = nn_pred3,
  denormalized = (nn_pred3 * normal_$std.dev) + normal_$mean)

prediksi4 <- data.frame(
  date = mlts_dum$dt[testing],
  normalized = nn_pred4,
  denormalized = (nn_pred4 * normal_$std.dev) + normal_$mean)


# visualisasi
ggplot(mydata_df,
       aes(x = date, y = export_growth)) +
  geom_line() + 
  geom_line(aes(y = denormalized), color = "tomato",
            data = prediksi3) +
  theme_tufte() + labs(x = "Date", y = "Export Growth",
                       title = "Forecasting Model BPNN [7,5,5,3]", 
                       caption = "Note: Forecasted value in red line")

ggplot(mydata_df,
       aes(x = date, y = export_growth)) +
  geom_line() + 
  geom_line(aes(y = denormalized), color = "tomato",
            data = prediksi4) +
  theme_tufte() + labs(x = "Date", y = "Export Growth",
                       title = "Forecasting Model BPNN [7,7,5,3]", 
                       caption = "Note: Forecasted value in red line")


# evaluasi atas model 3 dan model 4
eval3 <- cbind(mydata_df[62:73,], prediksi3)
eval3 <- eval3[,-c(3:5)]
rmse3 <- rmse(eval3$export_growth, eval3$denormalized)
mae3 <- mae(eval3$export_growth, eval3$denormalized)

eval4 <- cbind(mydata_df[62:73,], prediksi4)
eval4 <- eval4[,-c(3:5)]
rmse4 <- rmse(eval4$export_growth, eval4$denormalized)
mae4 <- mae(eval4$export_growth, eval4$denormalized)

accuracy_nn <- data.frame(ModelNN = (c("Model 3", "Model 4")),
                          RMSE = round(c(rmse3, rmse4), digits = 4),
                          MAE = round(c(mae3, mae4), digits = 4))

accuracy_nn %>%
  kbl() %>%
  kable_paper(full_width = T) %>%
  column_spec(2, color = "white",
              background = spec_color(1:2, end = 0.4, 
                                      option = "B", direction = -1),
              popover = paste("am:", accuracy_nn$am[1:2])) %>%
  column_spec(3, color = "white",
              background = spec_color(1:2, end = 0.4, 
                                      option = "B", direction = -1),
              popover = paste("am:", accuracy_nn$am[1:2]))


# gambar Neural Network Model 3
# WARNING! Gambar NN bisa saja tidak optimal
plot(nn_model3, information = T, dimension = 8)


#==== Peramalan ====

options(warn = -4)
mynewdata <- rbind(tail(mydata_df, 22),
                   data.frame(
                     date = seq(as.Date("2022-03-01"), 
                                by = "month", 
                                length.out = 12),
                     export_growth = NA,
                     normalized = NA
                   )); rownames(mynewdata) <- NULL


for(d in 23:nrow(mynewdata)) {
  newmlts_data <- mlts_transform(
    mynewdata[(d - 22):d, ],
    date, normalized, p = 5,
    extras = T, extrasAsFactors = T,
    granularity = "month")
  
  newmlts_data$mlts_extras_month <- month(newmlts_data$dt, label = T)
  
  newmlts_data <- cbind(
    newmlts_data[-1, ],
    dummy(newmlts_data[, "mlts_extras_month",
                       drop = F], 
          object = mlts_cat, int = T)[-1, ]
  )
  mynewdata$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model3, newmlts_data[, nn_fitur])$net.result
  )
}

mynewdata$export_growth <- (mynewdata$normalized * normal_$std.dev) +
  normal_$mean


# visualisasi
ggplot(filter(mydata_df), 
       aes(x = date, y = export_growth)) +
  geom_line(color = "chocolate2") + geom_point(color = "chocolate2")+
  geom_line(aes(y = export_growth), color = "dodgerblue3", linetype = 2,
            data = filter(mynewdata, date >= "2022-02-01")) +
  geom_point(data = filter(mynewdata, date >= "2022-02-01")) + 
  theme_solarized() + 
  labs(x = "Date", y = "Export Growth",
       title = 'Export Growth Forecasting',
       subtitle = "12-month (%)", 
       caption = "Noted: forecasted value in blue-dashed-line")

# nilai pengamatan hasil forecast dan visualisasinya
# data
mynewdata[, 1:2] %>% filter(date > "2022-02-01") %>%
  kbl() %>%
  kable_paper(full_width = T)

# plot
ggplot(filter(mynewdata, date > "2022-02-01"),
       aes(x = date, y = export_growth, 
           label = sprintf("%0.2f", round(export_growth, digits = 2))))+
  geom_line(size = 1.2, color = "dodgerblue3") + 
  geom_point(color = "navyblue") + 
  geom_text(nudge_y = 2, color = "navyblue") + 
  theme_economist(base_family = "helvetica", dkpanel = T) + 
  labs(x = "Date", y = "Export Growth",
       title = "Export Growth Forecasting",
       subtitle = "12 month, starting from March 2022")


#==== end ====








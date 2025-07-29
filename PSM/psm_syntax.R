#====Library dan Data====

# library
pacman::p_load(ggplot2, pricesensitivitymeter, kableExtra, dplyr, knitr, plyr,
               formattable, readxl, tinytex)

# data
mydata <- read_excel("data_psm.xlsx")

mydata %>% head(10) %>%
  kbl(caption = "Menampilkan Sampel 10 Data Hasil Responden", booktabs = T) %>%
  kable_paper(latex_options = c("striped", "HOLD_position"))



#====Preprocessing====
  
data <- mydata [,2:7]
names(data) <- c("Toocheap", "Cheap", "Expensive", "Tooexpensive",
                 "BuyCheap", "BuyExpensive")

data <- as.data.frame(data)
str(data)


#====Membentuk Model Price Sensitivity Meter====
  
psm_output <- psm_analysis(toocheap = "Toocheap", 
                           cheap = "Cheap" , 
                           expensive = "Expensive",
                           tooexpensive = "Tooexpensive",
                           pi_cheap = "BuyCheap",
                           validate = TRUE,
                           pi_expensive = "BuyExpensive",
                           data = data)

str(psm_output)

psm_output$data_input %>%
  head(10) %>% kbl(booktabs = T) %>%
  kable_paper(latex_options = c("striped", "HOLD_position"))

psm_output$data_vanwestendorp %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  head(10) %>%
  kbl(booktabs = T) %>%
  kable_paper(latex_options = c("striped", "HOLD_position"))


# estimated price, misalnya untuk harga 6000
psm_output$data_vanwestendorp[psm_output$data_vanwestendorp[,] == 6000,] %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  kbl(booktabs = T) %>%
  kable_paper(latex_options = c("striped", "HOLD_position"))


# estimasi gambaran harga 
price_resume <- list("Point Marginal Cheapness (PMC)" = psm_output$pricerange_lower, 
                     "Point of Marginal Expensiveness (PME)" =psm_output$pricerange_upper, 
                     "Indifference Price Point (IPP)" = psm_output$idp,
                     "Optimal Price Point (OPP)" = psm_output$opp)

price_table <- ldply(price_resume, data.frame)
names(price_table) <- c("Kategori Harga", "Harga")

kbl(price_table, booktabs = T) %>%
  kable_paper(latex_options = c("striped", "HOLD_position"), font_size = 18)


# visualisasi PSM
psmplot <-  ggplot(data = psm_output$data_vanwestendorp, aes(x = price)) +
  annotate(geom = "rect", 
           xmin = psm_output$pricerange_lower,
           xmax = psm_output$pricerange_upper,
           ymin = 0, ymax = Inf,
           fill="grey50", alpha = 0.3) +
  geom_line(aes(y = ecdf_toocheap,colour = "too cheap",
                linetype = "too cheap"), size = 0.8) +
  geom_line(aes(y = ecdf_tooexpensive, colour = "too expensive",
                linetype = "too expensive"), size = 0.8) + 
  geom_line(aes(y = ecdf_not_cheap, colour = "not cheap",
                linetype = "not cheap"), size = 0.8) +
  geom_line(aes(y = ecdf_not_expensive, colour = "not expensive",
                linetype = "not expensive"), size = 0.8) + 
  geom_line(aes(y = ecdf_cheap, colour = "cheap",
                linetype = "cheap"), size = 0.8) +
  geom_line(aes(y = ecdf_expensive, colour = "expensive",
                linetype = "expensive"), size = 0.8) +
  annotate(geom = "point", 
           x = psm_output$idp, 
           y = psm_output$data_vanwestendorp$ecdf_not_cheap[psm_output$data_vanwestendorp$price 
                                                            == psm_output$idp],
           size = 5,
           shape = 18,
           colour = "lightseagreen",
           fill = "lightseagreen",
           alpha = 0.8) + 
  annotate(geom = "point",
           x = psm_output$opp, 
           y = psm_output$data_vanwestendorp$ecdf_toocheap[psm_output$data_vanwestendorp$price ==
                                                             psm_output$opp],
           size = 5,
           shape = 18,
           colour = "forestgreen",
           fill = "forestgreen",
           alpha = 0.8) +
  annotate(geom = "point",
           x = psm_output$pricerange_lower,
           y = psm_output$data_vanwestendorp$ecdf_cheap[psm_output$data_vanwestendorp$price == 
                                                          psm_output$pricerange_lower],
           size = 5,
           shape = 15,
           colour = "khaki",
           fill = "khaki",
           alpha = 0.8) +
  annotate(geom = "point",
           x = psm_output$pricerange_upper,
           y = psm_output$data_vanwestendorp$ecdf_expensive[psm_output$data_vanwestendorp$price == 
                                                              psm_output$pricerange_upper],
           size = 5,
           shape = 15,
           colour = "mediumorchid3",
           fill = "mediumorchid3",
           alpha = 0.8)


psmplot +
  labs(x = "Harga",
       y = "Proporsi Responden",
       title = "Sensitivitas Harga untuk Produk XYZ",
       caption = paste("Sampel: ",
                       psm_output$total_sample - psm_output$invalid_cases,
                       "responden"))  + 
  scale_colour_manual(name = "Legenda",
                      values = c("too cheap" = "lightslateblue",
                                 "cheap" = "olivedrab2",
                                 "not cheap" = "sienna2",
                                 "not expensive" = "royalblue2",
                                 "expensive" = "goldenrod1",
                                 "too expensive" = "tomato2")) + 
  scale_linetype_manual(name = "Legenda",
                        values = c("too cheap" = "dotted",
                                   "cheap" = "dashed",
                                   "not cheap" = "solid",
                                   "not expensive" = "solid",
                                   "expensive" = "dashed",
                                   "too expensive" = "dotted")) + 
  annotate(geom = "text",
           x = psm_output$idp, 
           y = psm_output$data_vanwestendorp$ecdf_not_cheap[psm_output$data_vanwestendorp$price ==
                                                              psm_output$idp] + .05,
           label = paste("IDP: ", psm_output$idp)) + 
  annotate(geom = "text", 
           x = psm_output$opp,
           y = psm_output$data_vanwestendorp$ecdf_toocheap[psm_output$data_vanwestendorp$price ==
                                                             psm_output$opp] + .05,
           label = paste("OPP: ", psm_output$opp)) +
  annotate(geom = "text",
           x = psm_output$pricerange_lower,
           y = psm_output$data_vanwestendorp$ecdf_cheap[psm_output$data_vanwestendorp$price ==
                                                          psm_output$pricerange_lower] + 0.05,
           label = paste("PMC: ", psm_output$pricerange_lower)) +
  annotate(geom = "text",
           x = psm_output$pricerange_upper,
           y = psm_output$data_vanwestendorp$ecdf_expensive[psm_output$data_vanwestendorp$price ==
                                                              psm_output$pricerange_upper] + 0.05,
           label = paste("PME: ", psm_output$pricerange_upper)) +
  
  theme_minimal()


# optimizing price
head(psm_output$data_nms) %>%
  kbl(booktabs = T) %>%
  kable_paper(latex_options = c("striped", "HOLD_position"), font_size = 15)

price_optimum <- list("Harga Memaksimalkan Penerimaan pada Customer" = 
                        psm_output$price_optimal_trial, 
                      "Harga Memaksimalkan Pendapatan bagi Perusahaan" =
                        psm_output$price_optimal_revenue)

price_optimum_tab <- ldply(price_optimum, data.frame)
names(price_optimum_tab) <- c("Kategori Harga", "Harga")
price_optimum_tab %>%  
  kbl(booktabs = T) %>%
  kable_paper(latex_options = c("striped", "HOLD_position"), font_size = 18)


# Harga penerimaan maksimum pada customer
ggplot(data = psm_output$data_nms, aes(x = price)) + 
  geom_line(aes(y = trial), color = "tan1", size = 1) + 
  geom_vline(xintercept = psm_output$price_optimal_trial,
             linetype = "dotted", color = "gray30") + 
  geom_label(data = subset(psm_output$data_nms, trial == max(trial)),
             aes(x = price + 0.5, y = trial), 
             color = "black", fill = "lightgoldenrodyellow", alpha = 0.5,
             label = paste("Optimal Price:", psm_output$price_optimal_trial),
             hjust = 0) + 
  labs(x = "Harga", y = "Avg. Purhcase Probability",
       title = "Harga Memaksimalkan Penerimaan pada Customer", caption = paste("Sampel: ",
                                                                               psm_output$total_sample - psm_output$invalid_cases,
                                                                               "responden")) +
  theme_bw()


# harga memaksimalkan pendapatan
ggplot(data = psm_output$data_nms, aes(x = price)) + 
  geom_line(aes(y = revenue), color = "springgreen2", size = 1) +
  geom_vline(xintercept = psm_output$price_optimal_revenue,
             linetype = "dotted", color = "gray30") + 
  geom_label(data = subset(psm_output$data_nms, revenue == max(revenue)),
             aes(x = price + 0.5, y = revenue),
             color = "black", fill = "aquamarine", alpha = 0.5,
             label = paste("Optimal Price:", psm_output$price_optimal_revenue),
             hjust = 0) + 
  labs(x = "Harga", y = "Pendapatan",
       title = "Harga Memaksimalkan Pendapatan bagi Perusahaan",
       caption = paste("Sampel: ",
                       psm_output$total_sample - psm_output$invalid_cases,
                       "responden")) +
  theme_bw()

#====END====

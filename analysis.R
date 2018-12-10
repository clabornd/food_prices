source("requirements.R")

food_data <- read_csv("Data/WFPVAM_FoodPrices_24-7-2017.csv")

food_data <- food_data %>% mutate(Date = as.Date(paste(as.character(mp_year), as.character(mp_month), "1", sep = "-")))

syria_2017 <- food_data %>% filter(adm0_name == "Syrian Arab Republic", year(Date) == 2017)

food_data %>% group_by(Date, cm_id) %>%
  mutate(mean_price = mean(mp_price)) %>%
  ungroup() %>%
  filter(adm0_name == "Afghanistan", cm_name == "Bread") %>%
  ggplot(aes(Date, mean_price, col = adm0_name)) + geom_line() + theme(legend.position = "none")

View(distinct(food_data %>% group_by(Date, cm_id) %>%
                mutate(mean_price = mean(mp_price)) %>%
                ungroup() %>%
                filter(adm0_name == "Nepal" | adm0_name == "Afghanistan" | adm0_name == "India") %>% select(cm_name, adm0_name)))


india_rice <- food_data %>% group_by(Date, cm_id) %>%
  filter(adm0_name == "India", cm_name == "Rice") %>%
  mutate(mean_price = mean(mp_price)) %>%
  slice(1) %>%
  ungroup()

alldates <- data.frame(Date = seq(min(india_rice$Date), max(india_rice$Date), by = "month"))

india_rice <- alldates %>% left_join(india_rice)

india_rice$mean_price <- na.spline(india_rice$mean_price)

ts_india <- ts(india_rice$mean_price, start=c(1994, 1), end=c(2017, 6), frequency=12)

decomp <- stl(ts_india, s.window="periodic")
deseason <- seasadj(decomp)

fit <- auto.arima(deseason, seasonal = FALSE)
tsdisplay(residuals(fit))

fcast <- forecast(fit, h = 20)
plot(fcast)

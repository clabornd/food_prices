source("requirements.R")

food_data <- read_csv("Data/WFPVAM_FoodPrices_24-7-2017.csv")



#paste(unlist(simple_list), collapse = "|")

simple_list <- list("bread", "wheat", "rice", "wage", "milk", "beans", "potatoes", "meat", "lentils", "sugar", "oil", "fuel", "fish", "maize")

food_data_clean <- food_data %>% group_by(cm_name) %>% mutate(staples = strsplit(cm_name, split = " ")[[1]][1]) %>%
                  mutate(Date = as.Date(paste(as.character(mp_year), as.character(mp_month), "1", sep = "-"))) %>%
                  group_by(adm0_name, Date, staples) %>%
                  mutate(mean_price = mean(mp_price)) %>%
                  filter(grepl(paste(unlist(simple_list), collapse = "|"), staples, ignore.case = TRUE) == TRUE) %>%
                  ungroup() %>%
                  mutate(staples = tolower(staples))



write_csv(food_data_clean, "Data/food_data_clean.csv")



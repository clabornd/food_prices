source("requirements.R")

food_data <- read_csv("Data/WFPVAM_FoodPrices_24-7-2017.csv")

#paste(unlist(simple_list), collapse = "|")

food_data_clean <- food_data %>% group_by(cm_name) %>% mutate(staples = strsplit(cm_name, split = " ")[[1]][1])

write_csv(food_data_clean, "C:/Users/interns/Desktop")



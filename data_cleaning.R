################# 
# clean the data to be more ammenable to trelliscope #
# Get data from https://data.humdata.org/dataset/wfp-food-prices

source("requirements.R")

food_data <- read_csv("Data/wfpvam_foodprices.csv")

# lower case names of 'staple' foods
simple_list <- list("bread", "wheat", "rice", "wage", "milk", "beans", "potatoes", "meat", "lentils", "sugar", "oil", "fuel", "fish", "maize")

food_data_clean <- food_data %>% group_by(cm_name) %>% 
  mutate(staples = strsplit(cm_name, split = " ")[[1]][1]) %>%
  mutate(Date = as.Date(paste(as.character(mp_year), as.character(mp_month), "1", sep = "-"))) %>%
  group_by(adm0_name, Date, staples) %>%
  mutate(mean_price = mean(mp_price, na.rm = TRUE)) %>%
  filter(grepl(paste(unlist(simple_list), collapse = "|"), staples, ignore.case = TRUE) == TRUE) %>%
  group_by(adm0_name, staples) %>% 
  filter(Date > "2015-01-01") %>%
  filter(n() > 1000) %>% 
  ungroup() %>%
  mutate(staples = tolower(staples))

# food_data_clean <- food_data %>% group_by(cm_name) %>% 
#   mutate(staples = tolower(strsplit(cm_name, split = " ")[[1]][1])) %>%
#   mutate(Date = as.Date(paste(as.character(mp_year), as.character(mp_month), "1", sep = "-"))) %>%
#   group_by(adm0_name, Date, staples) %>%
#   summarise(mean_price = mean(mp_price, na.rm = TRUE)) %>%
#   group_by(adm0_name, staples) %>% 
#   filter(Date > "2015-01-01") %>%
#   filter(n() > 1000)

# test <- food_data %>% group_by(adm0_name) %>% nest()
  
write_csv(food_data_clean, "Data/food_data_2015-2018.csv")

###### TEST TRELLISCOPE
# food_data_clean %>% group_by(adm0_name)
# 
# test <- food_data %>% 
#   group_by(adm0_name, adm0_name) %>%
#   nest() %>%
#   mutate(plot = map_plot(data, price_v_time)) %>%
#   trelliscope("testdisplay", path = "./displays")

####


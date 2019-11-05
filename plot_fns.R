price_v_time <- function(df){
  ggplot(df, aes(x = Date, y = mp_price, group = Date)) + geom_boxplot(outlier.size = 1) +
  labs(y = "Price", x = "Date")
}

# 
# mean_price_v_time <- function(df){
#   ggplot(df, aes(x = Date, y = mean_price)) + geom_line() +
#     labs(y = "Price", x = "Date")
# }
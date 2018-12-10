price_v_time <- function(df){
  ggplot(df, aes(x = Date, y = mp_price, group = Date)) + geom_boxplot(outlier.size = 1) +
  labs(y = "Price", x = "Date")
}
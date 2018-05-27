#==================================================================
# @Project: Analyzing simulated home delivery platform orders data
# @Name: simulate_data
# @author: slow-data
# @date: 2018/05
#==================================================================

rm(list = ls()); gc()
library(lubridate)

## --- Simulate home-delivery order data
set.seed(123)
n <- 10000

# 1. order_id
order_id <- as.integer(10000001:10010000)

# 2. city_id
city_id <- as.factor(sample(x = c("NYC", "DAL", "DET"), size = n, replace = TRUE))

# 3. store_id
store_id <- as.integer(sample(x = 1000:1801, size = n, replace = TRUE))

# 4. total_purchase
idx_cheap <- as.integer(sample(x = 1:n, size = n/2, replace = FALSE))
idx_expensive <- !((1:n) %in% idx_cheap)
total_purchase <- rep(0,n) 
total_purchase[idx_cheap] <- sapply(rnorm(n/2, 10, 2), function(i) round(max(10,i),0))
total_purchase[idx_expensive] <- sapply(rnorm(sum(idx_expensive), 20, 5), function(i) round(max(15,i),0))

# 5. delivery_datetime
delivery_datetime <- sample(x = seq(as.POSIXlt("2017-01-01 00:00:01"), as.POSIXlt("2017-05-31 23:59:59"), by=1), size = n, replace = TRUE)
delivery_datetime <- as.POSIXlt(delivery_datetime)

# 6. year
year <- as.integer(year(delivery_datetime))

# 7. month
month <- as.integer(month(delivery_datetime))

# 8. day
day <- as.integer(day(delivery_datetime))

# 9. hour
hour <- as.integer(hour(delivery_datetime))

# 10. minute
minute <- as.integer(minute(delivery_datetime))

# 11. seconds
seconds <- as.integer(second(delivery_datetime))

# 12. delivery_date
delivery_date <- strftime(delivery_datetime, "%Y/%m/%d")

# 13. delivery_time
delivery_time <- strftime(delivery_datetime, "%H:%M:%S")

data <- data.frame(order_id = order_id, city_id = city_id, store_id = store_id, total_purchase = total_purchase, 
                   year = year, month = month, day = day, hour = hour, minute = minute, seconds = seconds,
                   delivery_date = delivery_date, delivery_time = delivery_time,
                   delivery_datetime = delivery_datetime)

# save workspace
rm(list=setdiff(ls(), "data"))
save.image("data/data.RData")





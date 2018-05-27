#==================================================================
# @Project: Analyzing simulated home delivery platform orders data
# @Name: analysis
# @author: slow-data
# @date: 2018/05
#==================================================================

rm(list=ls()); gc()
load("data/data.RData")
str(data)


# analysis by city and month
library(data.table)
dat <- as.data.table(data)
dat_city_month <- dat[,.(purchase=sum(total_purchase), n_orders=.N), by=.(city_id,month)]
dat_city_month$avg_order <- dat_city_month$purchase/dat_city_month$n_orders

library(ggplot2)
g <- ggplot(data = dat_city_month, aes(x = month, y = n_orders, fill = city_id)) 
g + geom_bar(stat = "identity")


# analysis by store and month
dat3 <- dat[,.(purchase=sum(total_purchase), n_orders=.N, n_stores = length(unique(store_id)) ), by=.(city_id,month)]
dat3$avg_order <- dat3$purchase/dat3$n_orders

st1 <- ggplot(data = dat3, aes(x = month, y = n_stores, fill = city_id)) 
st1 + geom_bar(stat = "identity") +
  labs(title="Store acquisition evolution",
     x="Month 2017", y="N. Stores") +
  scale_x_continuous(breaks=1:6) +
  theme(plot.title = element_text(hjust = 0, vjust=2, size = 16, family = "Times"),
        axis.text.x = element_text(size = 14, family = "Times"),
        axis.text.y = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey")        
  ) +
  guides(fill=guide_legend(title="CITY"))

  



# --------------------------------------
# NYC
# --------------------------------------


## Filter only NYC
nyc <- data[data$city_id=="NYC",]

# aggregate data by store
nyc_dat <- as.data.table(nyc)
nyc_sum <- nyc_dat[,.(purchase=sum(total_purchase), n_orders=.N), by=store_id]
nyc_sum$avg_order <- nyc_sum$purchase/nyc_sum$n_orders

# summary distribution
summary(nyc_sum)

# outliers? special partners
quantile(nyc_sum$purchase, 0.95)
quantile(nyc_sum$purchase, 0.99)
nyc_sum[nyc_sum$purchase>quantile(nyc_sum$purchase, 0.99),]
nyc_sum[nyc_sum$n_orders>quantile(nyc_sum$n_orders, 0.999),]

nyc_sum_noout <- nyc_sum[nyc_sum$n_orders<=quantile(nyc_sum$n_orders, 0.999),]


# plot store purchase distribution
g <- ggplot(nyc_sum, aes(purchase))
g + geom_density(fill="wheat", alpha=0.8) + 
  labs(title="Store purchases - Density plot", 
       caption="Source: NYC data, HY 2017 (99th percentile not displayed)",
       x="", y="") +
  xlim(0, quantile(nyc_sum$purchase, 0.99))

qplot(store_id, purchase, data = nyc_sum, xlab = "Store id", ylab = "Overall HY2017 purchases")

# plot store N. order distribution
g2 <- ggplot(nyc_sum, aes(n_orders))
g2 + geom_density(fill="light yellow", alpha=0.8) + 
  labs(title="Store n. orders - Density plot", 
       caption="Source: NYC data, HY 2017 (99,5th percentile not displayed)",
       x="", y="") +
  xlim(0, quantile(nyc_sum$n_orders, 0.995)) 


# plot store average value order distribution
g3 <- ggplot(nyc_sum, aes(avg_order))
g3 + geom_density(fill="light yellow", alpha=0.8) + 
  labs(title="Store average order value - Density plot", 
       caption="Source: NYC data, HY 2017",
       x="", y="") 

# scatter plot
qplot(n_orders, avg_order, data = nyc_sum)
qplot(avg_order, n_orders, data = nyc_sum)

# assign ranks (N_orders)
quantile(nyc_sum$n_orders, probs=c(0, 0.5, 0.95, 1))
nyc_sum <- setDT(nyc_sum)[, rank_n_order := cut(n_orders, quantile(n_orders, probs=c(0, 0.5, 0.95, 1)), include.lowest=TRUE, labels=FALSE)]
table(nyc_sum$rank_n_order)
sum(table(nyc_sum$rank_n_order))

# assign ranks (avg_order)
quantile(nyc_sum$avg_order, probs=c(0, 0.5, 0.95, 1))
nyc_sum <- setDT(nyc_sum)[, rank_avg_value := cut(avg_order, quantile(avg_order, probs=c(0, 0.5, 0.95, 1)), include.lowest=TRUE, labels=FALSE)]
table(nyc_sum$rank_avg_value)
sum(table(nyc_sum$rank_avg_value))

# create classification on num_orders
nyc_sum$class_num[nyc_sum$rank_n_order==1] <- "LF"
nyc_sum$class_num[nyc_sum$rank_n_order==2] <- "MF"
nyc_sum$class_num[nyc_sum$rank_n_order==3] <- "HF"
nyc_sum$class_num <- factor(nyc_sum$class_num, levels = c("HF", "MF", "LF"), ordered = TRUE)
table(nyc_sum$class_num)

# create classification on avg_val
nyc_sum$class_val[nyc_sum$rank_avg_value==3] <- "GOLD"
nyc_sum$class_val[nyc_sum$rank_avg_value==2] <- "SILVER"
nyc_sum$class_val[nyc_sum$rank_avg_value==1] <- "BRONZE"
table(nyc_sum$class_val)
nyc_sum$class_val <- factor(nyc_sum$class_val, levels = c("GOLD", "SILVER", "BRONZE"), ordered = TRUE)
table(nyc_sum$class_val)

# combination classes
nyc_sum$class <- paste(nyc_sum$class_num, " / ", nyc_sum$class_val)
table(nyc_sum$class)
sum(table(nyc_sum$class))
nyc_sum$class <- factor(nyc_sum$class, levels = c("HF  /  GOLD", "HF  /  SILVER", "HF  /  BRONZE", "MF  /  GOLD", "MF  /  SILVER", "MF  /  BRONZE", "LF  /  GOLD", "LF  /  SILVER", "LF  /  BRONZE"), ordered = TRUE)
table(nyc_sum$class)
sum(table(nyc_sum$class))  


# scatter plot
nyc_sum_noout2 <- nyc_sum[nyc_sum$n_orders<=quantile(nyc_sum$n_orders, 0.999),]
q1 <- qplot(n_orders, avg_order, data = nyc_sum_noout2, color = class_val, xlab = "N. of orders", ylab = "Average order value")

q1 + theme(legend.text = element_text(face = "italic")) +
  scale_colour_discrete(name = "Classification",
                        guide = guide_legend(override.aes = list(size = 5))) +
  theme(legend.key.size = unit(1, "cm"))

qplot(n_orders, avg_order, data = nyc_sum_noout2, color = class_num, xlab = "N. of orders", ylab = "Average order value") +
  scale_colour_discrete(name = "Classification", guide = guide_legend(override.aes = list(size = 5)))
  
qplot(n_orders, avg_order, data = nyc_sum_noout2, color = class, xlab = "N. of orders", ylab = "Average order value") +
  scale_colour_discrete(name = "Classification", guide = guide_legend(override.aes = list(size = 5)))


# --------------------------------------
# INtra day study
# --------------------------------------

# daypart classification
nyc$daypart[nyc$hour>=7&nyc$hour<14] <- "Morning" 
nyc$daypart[nyc$hour>=14&nyc$hour<20] <- "Afternoon" 
nyc$daypart[nyc$hour>=20&nyc$hour<=23] <- "Evening"
nyc$daypart[nyc$hour>=0&nyc$hour<=6] <- "Night"

# meals classification
nyc$daypart2[nyc$hour>=9&nyc$hour<13] <- "Office" 
nyc$daypart2[nyc$hour>=13&nyc$hour<16] <- "Lunch" 
nyc$daypart2[nyc$hour>=16&nyc$hour<=19] <- "Service"
nyc$daypart2[nyc$hour>19&nyc$hour<=23|nyc$hour==0] <- "Dinner"
nyc$daypart2[nyc$hour>0&nyc$hour<=8] <- "Night"

# summarize data
nyc_hour <- nyc_dat[,.(purchase=sum(total_purchase), n_orders=.N), by=hour]
nyc_hour$avg_order <- nyc_hour$purchase/nyc_hour$n_orders

windowsFonts(Impact=windowsFont("Impact"))
windowsFonts(Times=windowsFont("TT Times New Roman"))

nyc_hour$hour2 <- nyc_hour$hour
nyc_hour$hour2[nyc_hour$hour==0] <- 24

d1 <- ggplot(nyc_hour, aes(x = hour2, y = purchase/1000)) +
  geom_bar(stat = "identity", color = "black", fill = "darkseagreen3") +
#  coord_flip() +
  xlab("") +
  ylab("Purchase (.000 ???)") +
  ggtitle("") +
  scale_x_continuous(breaks=1:24) +
  theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
        axis.text.x = element_text(size = 14, family = "Times"),
        axis.text.y = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey")        
  ) 

d1


d2 <- ggplot(nyc_hour, aes(x = hour2, y = n_orders)) +
  geom_bar(stat = "identity", color = "black", fill = "darkolivegreen2") +
  #  coord_flip() +
  xlab("") +
  ylab("N. orders") +
  ggtitle("") +
  scale_x_continuous(breaks=1:24) +
  theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
        axis.text.x = element_text(size = 14, family = "Times"),
        axis.text.y = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey")        
  ) 

d2


d3 <- ggplot(nyc_hour, aes(x = hour2, y = avg_order)) +
  geom_bar(stat = "identity", color = "black", fill = "darkolivegreen2") +
  #  coord_flip() +
  xlab("") +
  ylab("Avg. order value") +
  ggtitle("") +
  scale_x_continuous(breaks=1:24) +
  theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
        axis.text.x = element_text(size = 14, family = "Times"),
        axis.text.y = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey")        
  ) 

d3




#### Intra-day study all data

# summarize data
data_hour <- dat[,.(purchase=sum(total_purchase), n_orders=.N), by=.(hour, city_id)]
data_hour$avg_order <- data_hour$purchase/data_hour$n_orders

windowsFonts(Impact=windowsFont("Impact"))
windowsFonts(Times=windowsFont("TT Times New Roman"))

data_hour$hour2 <- data_hour$hour
data_hour$hour2[data_hour$hour==0] <- 24

data_hour_nyc <- data_hour[data_hour$city_id=="NYC",]
data_hour_nyc$perc <- data_hour_nyc$n_orders/sum(data_hour_nyc$n_orders)
head(data_hour_nyc)

data_hour_dal <- data_hour[data_hour$city_id=="DAL",]
data_hour_dal$perc <- data_hour_dal$n_orders/sum(data_hour_dal$n_orders)

data_hour_det <- data_hour[data_hour$city_id=="DET",]
data_hour_det$perc <- data_hour_det$n_orders/sum(data_hour_det$n_orders)

brks <- c(0, 25, 50, 75, 100)
z1 <- ggplot(data_hour_nyc, aes(x = hour2, y = perc*100)) +
  geom_bar(stat = "identity") +
  #  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  #  coord_flip() +
  xlab("") +
  ylab("N. orders (%)") +
  ggtitle("") +
  scale_x_continuous(breaks=1:24) +
  ylim(c(0,25))
theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
      axis.text.x = element_text(size = 14, family = "Times"),
      axis.text.y = element_text(size = 14, family = "Times"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_line(colour = "grey")        
) 

z1


z2 <- ggplot(data_hour_dal, aes(x = hour2, y = perc*100)) +
  geom_bar(stat = "identity") +
  #  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  #  coord_flip() +
  xlab("") +
  ylab("N. orders (%)") +
  ggtitle("") +
  scale_x_continuous(breaks=1:24) +
  ylim(c(0,25))
theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
      axis.text.x = element_text(size = 14, family = "Times"),
      axis.text.y = element_text(size = 14, family = "Times"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_line(colour = "grey")        
) 

z2


z3 <- ggplot(data_hour_det, aes(x = hour2, y = perc*100)) +
  geom_bar(stat = "identity") +
  #  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +  #  coord_flip() +
  xlab("") +
  ylab("N. orders (%)") +
  ggtitle("") +
  scale_x_continuous(breaks=1:24) +
  ylim(c(0,25))
  theme(plot.title = element_text(hjust = 0, vjust=5, size = 20, family = "Times"),
        axis.text.x = element_text(size = 14, family = "Times"),
        axis.text.y = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey")        
  ) 

z3

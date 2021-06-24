
library("ggplot2")
library("dplyr")
library(tidyr)
library("zoo")
install.packages("readxl")
library("readxl")
library(reshape2)
install.packages("panelr")
library(panelr)
library(data.table)


my_data <- read_excel("BankDataExample1.xlsx", sheet = "1_Àêòèâû")
my_data1 <- as.data.frame(my_data[c(3:nrow(my_data)), ])


my_data2 <- my_data1[, -c(1)]
dates <- as.Date(as.numeric(my_data2[c(1),]), origin = "1899-12-30")


my_data3 <- as.data.frame(my_data[c(4:nrow(my_data)), ])
REGN <- my_data3[, 1]
REGN <- REGN[-length(REGN)]


my_data4 <- my_data3[, -c(1)]
colnames(my_data4) <- paste("assets", dates, sep = "_")
my_data4 <- head(my_data4, -1)
my_data4$REGN <- REGN 

data <- reshape(my_data4, 
                direction = "long",
                varying = list(names(my_data4)[1:199]),
                v.names = "TA",
                idvar = c("REGN"),
                timevar = "Date",
                times = dates)
rownames(data) <- NULL

data <- data[str_order(data[,1]),]
data$TA <- as.numeric(data$TA)/1000000
data_TA <- data




my_data <- read_excel("BankDataExample1.xlsx", sheet = "3_ÊðåäÞË")
my_data1 <- as.data.frame(my_data[c(3:nrow(my_data)), c(1:(ncol(my_data) - 3))])


my_data2 <- my_data1[, -c(1)]
dates <- as.Date(as.numeric(my_data2[c(1),]), origin = "1899-12-30")


my_data3 <- as.data.frame(my_data[c(4:nrow(my_data)), c(1:(ncol(my_data) - 3))])
REGN <- my_data3[, 1]


my_data4 <- my_data3[, -c(1)]
colnames(my_data4) <- paste("LNSF", dates, sep = "_")
my_data4$REGN <- REGN 

my_data4 <- head(my_data4, -5)

data <- reshape(my_data4, 
                direction = "long",
                varying = list(names(my_data4)[1:199]),
                v.names = "LNSF",
                idvar = c("REGN"),
                timevar = "Date",
                times = dates)
rownames(data) <- NULL

data <- data[str_order(data[,1]),]
data$LNSF <- as.numeric(data$LNSF)/1000000
data_LNSF <- data




my_data <- read_excel("BankDataExample1.xlsx", sheet = "4_ÏðîñÊðåäÞË")
my_data <- my_data[, -c(ncol(my_data))]
my_data1 <- as.data.frame(my_data[c(3:nrow(my_data)), ])


my_data2 <- my_data1[, -c(1)]
dates <- as.Date(as.numeric(my_data2[c(1),]), origin = "1899-12-30")


my_data3 <- as.data.frame(my_data[c(4:nrow(my_data)), ])
REGN <- my_data3[, 1]
REGN <- REGN[-length(REGN)]


my_data4 <- my_data3[, -c(1)]
colnames(my_data4) <- paste("NPLF", dates, sep = "_")
my_data4 <- head(my_data4, -1)
my_data4$REGN <- REGN 

data <- reshape(my_data4, 
                direction = "long",
                varying = list(names(my_data4)[1:199]),
                v.names = "NPLF",
                idvar = c("REGN"),
                timevar = "Date",
                times = dates)
rownames(data) <- NULL

data <- data[str_order(data[,1]),]
data$NPLF <- as.numeric(data$NPLF)/1000000
data_NPLF <- data




my_data <- read_excel("BankDataExample1.xlsx", sheet = "5_ÊðåäÔË")
my_data <- my_data[, -c(ncol(my_data))]
my_data1 <- as.data.frame(my_data[c(3:nrow(my_data)), ])


my_data2 <- my_data1[, -c(1)]
dates <- as.Date(as.numeric(my_data2[c(1),]), origin = "1899-12-30")


my_data3 <- as.data.frame(my_data[c(4:nrow(my_data)), ])
REGN <- my_data3[, 1]
REGN <- REGN[-length(REGN)]


my_data4 <- my_data3[, -c(1)]
colnames(my_data4) <- paste("LNSH", dates, sep = "_")
my_data4 <- head(my_data4, -1)
my_data4$REGN <- REGN 

data <- reshape(my_data4, 
                direction = "long",
                varying = list(names(my_data4)[1:199]),
                v.names = "LNSH",
                idvar = c("REGN"),
                timevar = "Date",
                times = dates)
rownames(data) <- NULL

data <- data[str_order(data[,1]),]
data$LNSH <- as.numeric(data$LNSH)/1000000
data_LNSH <- data




my_data <- read_excel("BankDataExample1.xlsx", sheet = "6_ÏðîñÊðåäÔË")
my_data <- my_data[, -c(ncol(my_data))]
my_data1 <- as.data.frame(my_data[c(3:nrow(my_data)), ])


my_data2 <- my_data1[, -c(1)]
dates <- as.Date(as.numeric(my_data2[c(1),]), origin = "1899-12-30")


my_data3 <- as.data.frame(my_data[c(4:nrow(my_data)), ])
REGN <- my_data3[, 1]
REGN <- REGN[-length(REGN)]


my_data4 <- my_data3[, -c(1)]
colnames(my_data4) <- paste("NPLH", dates, sep = "_")
my_data4 <- head(my_data4, -1)
my_data4$REGN <- REGN 

data <- reshape(my_data4, 
                direction = "long",
                varying = list(names(my_data4)[1:199]),
                v.names = "NPLH",
                idvar = c("REGN"),
                timevar = "Date",
                times = dates)
rownames(data) <- NULL

data <- data[str_order(data[,1]),]
data$NPLH <- as.numeric(data$NPLH)/1000000
data_NPLH <- data



my_data <- read_excel("BankDataExample1.xlsx", sheet = "29_ÁàëÏðèáûë_y")
my_data1 <- as.data.frame(my_data[c(3:nrow(my_data)), ])


my_data2 <- my_data1[, -c(1)]
dates <- as.Date(as.numeric(my_data2[c(1),]), origin = "1899-12-30")
dates[c(2:13)] <- c("2004-02-01", "2004-03-01", "2004-04-01", "2004-05-01", "2004-06-01", "2004-07-01", "2004-08-01", "2004-09-01", "2004-10-01","2004-11-01", "2004-12-01", "2005-01-01")


my_data3 <- as.data.frame(my_data[c(4:nrow(my_data)), ])
REGN <- my_data3[, 1]
REGN <- REGN[-length(REGN)]


my_data4 <- my_data3[, -c(1)]
colnames(my_data4) <- paste("PR", dates, sep = "_")
my_data4 <- head(my_data4, -1)
my_data4$REGN <- REGN 

data <- reshape(my_data4, 
                direction = "long",
                varying = list(names(my_data4)[1:199]),
                v.names = "PR",
                idvar = c("REGN"),
                timevar = "Date",
                times = dates)
rownames(data) <- NULL

data <- data[str_order(data[,1]),]
data$PR <- as.numeric(data$PR)/1000000
data_PR <- data



data_prob <- merge(data_TA, data_LNSF, by = c("Date", "REGN"), all = TRUE)
data_prob <- merge(data_prob, data_NPLF, by = c("Date", "REGN"), all = TRUE)
data_prob <- merge(data_prob,  data_LNSH, by = c("Date", "REGN"), all = TRUE)
data_prob <- merge(data_prob, data_NPLH, by = c("Date", "REGN"), all = TRUE)
data_prob <- merge(data_prob, data_PR, by = c("Date", "REGN"), all = TRUE)
data_prob <- data_prob[str_order(data_prob[,2]),]




t = seq(as.Date("2007-01-01"), by= "3 month", length.out= 55)

data_final <- subset(data_prob, data_prob$Date %in% t)

data_final1 <- data_final[, c(3:8)]

keep <- Reduce(`&`, lapply(data_final1, function(x) x >= quantile(x, .01, na.rm = TRUE) 
                           & x <= quantile(x, .99, na.rm = TRUE)))
data_final2 <- data_final[keep,]
data_final2$logTA <- log(data_final2$TA)
data_final2$TAgrowth <- data_final2$TA/lag(data_final2$TA, k =4)

interval_size <- '3 month'
wanted_percentiles <- c(0.15, 0.25, 0.5, 0.75, 0.85)

get_quantiles <- function(items) {
  yy <- quantile(items, wanted_percentiles)
  return (yy)
}

r <- aggregate(logTA ~ cut(Date, interval_size), data_final2, get_quantiles)
names(r) <- c("ts", "t")
df <- as.data.frame(r$t)
df$ts <- r$ts
df$ts <- as.Date(df$ts)

ggplot(df, aes(x = ts)) +
  geom_line(aes(y = `15%`, col="15%")) +
  geom_line(aes(y = `25%`, col="25%")) +
  geom_line(aes(y = `50%`, col="50%")) +
  geom_line(aes(y = `75%`, col="75%")) +
  geom_line(aes(y = `85%`, col="85%")) +
  labs(x = "Date", y = "logTA") +
  ggtitle("logTA percentiles over time in 3 months intervals")




r <- aggregate(TAgrowth ~ cut(Date, interval_size), data_final2, get_quantiles)
names(r) <- c("ts", "t")
df <- as.data.frame(r$t)
df$ts <- r$ts
df$ts <- as.Date(df$ts)

ggplot(df, aes(x = ts)) +
  geom_line(aes(y = `15%`, col="15%")) +
  geom_line(aes(y = `25%`, col="25%")) +
  geom_line(aes(y = `50%`, col="50%")) +
  geom_line(aes(y = `75%`, col="75%")) +
  geom_line(aes(y = `85%`, col="85%")) +
  labs(x = "Date", y = "TAgrowth") +
  ggtitle("TAgrowth percentiles over time in 3 months intervals")


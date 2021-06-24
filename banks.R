####################
#    Magzhanov     #
#    31/05/2021    #
#    Banks PS#1    #
####################



##### PACKAGES #####

install.packages("readxl")
install.packages("panelr")
install.packages("robustHD")
install.packages("DescTools")
install.packages("lmtest")
install.packages("frontier")

library("ggplot2")
library("dplyr")
library(tidyr)
library("zoo")
library("readxl")
library(reshape2)
library(panelr)
library(data.table)
library("xlsx")
library(robustHD)
library(DescTools)
library("lmtest")
library(frontier)

##### TASK 1 (DATA MANIPULATION) #####

# A procedure is the same for all the lists: 
# 1. Read excel sheet 
# 2. Take proper cells and get proper REGN
# 3. Reshape from Wide panel to Long panel 
# 4. Order by REGN and convert to bln. rubles

# TA
# Steps 1. and 2.
my_data <- read_excel("BankDataExample1.xlsx", sheet = 2)
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

# Step 3.
data <- reshape(my_data4, 
        direction = "long",
        varying = list(names(my_data4)[1:199]),
        v.names = "TA",
        idvar = c("REGN"),
        timevar = "Date",
        times = dates)
rownames(data) <- NULL

#Step 4. 
data <- data[str_order(data[,1]),]
data$TA <- as.numeric(data$TA)/1000000
data_TA <- data


# LNSF
# Steps 1. and 2.
my_data <- read_excel("BankDataExample1.xlsx", sheet = 3)
my_data1 <- as.data.frame(my_data[c(3:nrow(my_data)), c(1:(ncol(my_data) - 3))])


my_data2 <- my_data1[, -c(1)]
dates <- as.Date(as.numeric(my_data2[c(1),]), origin = "1899-12-30")


my_data3 <- as.data.frame(my_data[c(4:nrow(my_data)), c(1:(ncol(my_data) - 3))])
REGN <- my_data3[, 1]


my_data4 <- my_data3[, -c(1)]
colnames(my_data4) <- paste("LNSF", dates, sep = "_")
my_data4$REGN <- REGN 

my_data4 <- head(my_data4, -5)

# Step 3.
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


# NPLF
# Steps 1. and 2.
my_data <- read_excel("BankDataExample1.xlsx", sheet = 4)
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

# Step 3.
data <- reshape(my_data4, 
                direction = "long",
                varying = list(names(my_data4)[1:199]),
                v.names = "NPLF",
                idvar = c("REGN"),
                timevar = "Date",
                times = dates)
rownames(data) <- NULL

#Step 4.
data <- data[str_order(data[,1]),]
data$NPLF <- as.numeric(data$NPLF)/1000000
data_NPLF <- data


# LNSH
# Steps 1. and 2.
my_data <- read_excel("BankDataExample1.xlsx", sheet = 5)
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

# Step 3.
data <- reshape(my_data4, 
                direction = "long",
                varying = list(names(my_data4)[1:199]),
                v.names = "LNSH",
                idvar = c("REGN"),
                timevar = "Date",
                times = dates)
rownames(data) <- NULL

#Step 4.
data <- data[str_order(data[,1]),]
data$LNSH <- as.numeric(data$LNSH)/1000000
data_LNSH <- data


# NPLH
# Steps 1. and 2.
my_data <- read_excel("BankDataExample1.xlsx", sheet = 6)
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

# Step 3.
data <- reshape(my_data4, 
                direction = "long",
                varying = list(names(my_data4)[1:199]),
                v.names = "NPLH",
                idvar = c("REGN"),
                timevar = "Date",
                times = dates)
rownames(data) <- NULL

#Step 4.
data <- data[str_order(data[,1]),]
data$NPLH <- as.numeric(data$NPLH)/1000000
data_NPLH <- data


# PR
# Steps 1. and 2.

my_data <- read_excel("BankDataExample1.xlsx", sheet = 8)
my_data1 <- as.data.frame(my_data[c(3:nrow(my_data)), ])


my_data2 <- my_data1[, -c(1)]
dates <- as.Date(as.numeric(my_data2[c(1),]), origin = "1899-12-30")

# Some problem arised with reading dates, so solve it manually: 
dates[c(2:13)] <- c("2004-02-01", "2004-03-01", "2004-04-01", "2004-05-01", "2004-06-01", "2004-07-01", "2004-08-01", "2004-09-01", "2004-10-01","2004-11-01", "2004-12-01", "2005-01-01")

my_data3 <- as.data.frame(my_data[c(4:nrow(my_data)), ])
REGN <- my_data3[, 1]
REGN <- REGN[-length(REGN)]


my_data4 <- my_data3[, -c(1)]
colnames(my_data4) <- paste("PR", dates, sep = "_")
my_data4 <- head(my_data4, -1)
my_data4$REGN <- REGN 

# Step 3.
data <- reshape(my_data4, 
                direction = "long",
                varying = list(names(my_data4)[1:199]),
                v.names = "PR",
                idvar = c("REGN"),
                timevar = "Date",
                times = dates)
rownames(data) <- NULL

#Step 4.
data <- data[str_order(data[,1]),]
data$PR <- as.numeric(data$PR)/1000000
data_PR <- data


# Merge variables:

data_prob <- merge(data_TA, data_LNSF, by = c("Date", "REGN"), all = TRUE)
data_prob <- merge(data_prob, data_NPLF, by = c("Date", "REGN"), all = TRUE)
data_prob <- merge(data_prob,  data_LNSH, by = c("Date", "REGN"), all = TRUE)
data_prob <- merge(data_prob, data_NPLH, by = c("Date", "REGN"), all = TRUE)
data_prob <- merge(data_prob, data_PR, by = c("Date", "REGN"), all = TRUE)
data_prob <- data_prob[str_order(data_prob[,2]),]


# Finalize dataset: 

t = seq(as.Date("2007-01-01"), by= "3 month", length.out= 55)
data_final <- subset(data_prob, data_prob$Date %in% t)
data_final$REGN <- as.numeric(data_final$REGN)
data_final <- data_final[order(data_final[,2]),]

data_final$TA <- as.numeric(data_final$TA)
data_final$LNSF <- as.numeric(data_final$LNSF)
data_final$NPLF <- as.numeric(data_final$NPLF)
data_final$LNSH <- as.numeric(data_final$LNSH)
data_final$NPLH <- as.numeric(data_final$NPLH)
data_final$PR <- as.numeric(data_final$PR)

# Save dataset:

write.csv(data_final, "data_final.csv")

# Get quantiles table: 

do.call("rbind",
        tapply(data_final$TA,       
               data_final$LNSF,  
               data_final$NPLF,
               data_final$LNSH,
               data_final$NPLH,
               data_final$PR,
               quantile(x, probs = c(0.005, 0.01, 0.02, 0.15, 0.85, 0.98, 0.99, 0.995), na.rm = TRUE)))
quants = c(0.005, 0.01, 0.02, 0.15, 0.85, 0.98, 0.99, 0.995)

stargazer(apply(data_final[3:8] , 2 , quantile , probs = quants , na.rm = TRUE))

##### CHECK UP (FINE) #####

# Upload dataset from PANEL sheet: 

check <- read.csv("check.csv",
                    header = TRUE, sep = ";", stringsAsFactors = FALSE)

# Check for TA:
for (t in 1:length(check$TA)) {
  
  if (identical(check$TA[t],data_final$TA[t]) == FALSE){
  print(t)
  }
}

check$TA[2598]
data_final$TA[2598]

# One little problem: digits matter

data_final$TA <- format(round(data_final$TA, 5), nsmall = 5)
check$TA <- format(round(check$TA, 5), nsmall = 5)

for (t in 1:length(check$TA)) {
  
  if (identical(check$TA[t],data_final$TA[t]) == FALSE){
    print(t)
  }
}

check$TA[13194]
data_final$TA[13194]

# OK it's FINE

# Finalize dataset: 
t = seq(as.Date("2007-01-01"), by= "3 month", length.out= 55)
data_final <- subset(data_prob, data_prob$Date %in% t)
data_final$REGN <- as.numeric(data_final$REGN)
data_final <- data_final[order(data_final[,2]),]
data_final$TA <- as.numeric(data_final$TA)
data_final$LNSF <- as.numeric(data_final$LNSF)
data_final$NPLF <- as.numeric(data_final$NPLF)
data_final$LNSH <- as.numeric(data_final$LNSH)
data_final$NPLH <- as.numeric(data_final$NPLH)
data_final$PR <- as.numeric(data_final$PR)


##### TASK 2 (GRAPHS AND REGRESSIONS) #####

library(plm)

##### SIMPLE (FLAT CLEANING: ALL YEARS) + PLOTS: #####

# NAs to zeros if bank operates:
data_final4 <- data_final
data_final4[is.na(data_final4) & data_final4$TA!=0] <- 0
data_final4 = subset(data_final4, data_final4$TA > 0)

# ADD NEW VARIABLES:
data_final4$logTA <- log(data_final4$TA)
data_final4$ROA<-(data_final4$PR/data_final4$TA)*100
data_final4$CLTA<-data_final4$LNSF/data_final4$TA
data_final4$RLTA<-data_final4$LNSH/data_final4$TA
data_final4$NPLs<-(data_final4$NPLH+data_final4$NPLF)/(data_final4$LNSF+data_final4$LNSH)


# Get boolean dataset of non-outliers:
keep <- Reduce(`&`, lapply(data_final4[, c(3:8)], function(x) x >= quantile(x, .01, na.rm = TRUE) 
                           & x <= quantile(x, .99, na.rm = TRUE)))
# Keep non-outliers:
data_final4 <- data_final4[keep,]

# Make panel dataset:
data_final4 <- pdata.frame(data_final4,
                           index = c("REGN", "Date"),
                           row.names = TRUE)

# Add variables:
data_final4$Date<-as.Date(as.character(data_final4$Date))
data_final4$TAgrowth <- (data_final4$TA/lag(data_final4$TA, k =4))

# Finalize dataset:
data_final1 <- data_final4


# Get plots: 
interval_size <- '3 month'
wanted_percentiles <- c(0.15, 0.25, 0.5, 0.75, 0.85)

get_quantiles <- function(items) {
  yy <- quantile(items, wanted_percentiles)
  return (yy)
}

r <- aggregate(logTA ~ cut(Date, interval_size), data_final1, get_quantiles)
names(r) <- c("ts", "t")
df <- as.data.frame(r$t)
df$ts <- r$ts
df$ts <- as.Date(df$ts)

crisis08 <- data.frame(xmin=as.Date('2008-07-01'), xmax=as.Date('2010-07-01'), ymin=-Inf, ymax=Inf)
crisis14 <- data.frame(xmin=as.Date('2014-01-01'), xmax=as.Date('2015-07-01'), ymin=-Inf, ymax=Inf)

ggplot(df, aes(x = ts)) +
  geom_line(aes(y = `15%`, col="15%")) +
  geom_line(aes(y = `25%`, col="25%")) +
  geom_line(aes(y = `50%`, col="50%")) +
  geom_line(aes(y = `75%`, col="75%")) +
  geom_line(aes(y = `85%`, col="85%")) +
  labs(x = "Date", y = "logTA") +
  geom_rect(data=crisis08, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3) +
  geom_rect(data=crisis14, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3)+
  ggtitle("log(TA) percentiles in 3 months intervals (flat cleaning)")

r <- aggregate(TAgrowth ~ cut(Date, interval_size), data_final1, get_quantiles)
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
  geom_rect(data=crisis08, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3) +
  geom_rect(data=crisis14, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3)+
  ggtitle("TA growth percentiles in 3 months intervals (flat cleaning)")


##### SMART (WINSORIZE PANEL DATA BY YEAR) + PLOTS: #####

library(plm)


## determine unique values in Dates and their length
unique_dates <- unique(data_final$Date)
n_unique_dates <- length(unique_dates)


# WINSORIZE TA
## create an empty list of length Dates

data_final.TA <- vector("list", length=n_unique_dates)

for(i in 1:n_unique_dates){
  data_final.TA[[i]]  <- Winsorize(subset(data_final, Date==unique_dates[i], 
                                       select=c(TA)), na.rm = TRUE, probs = c(0.01, 0.99))
  
  ## add the date field to each winsorized data frame
  data_final.TA[[i]] <- cbind(data_final.TA[[i]], Date=unique_dates[i], REGN = REGN)
}

dTA <- bind_rows(data_final.TA)

dTA$REGN <- as.numeric(dTA$REGN)
dTA <- dTA[order(dTA[,2]),]


# WINSORIZE LNSF
## create an empty list of length Dates

data_final.LNSF <- vector("list", length=n_unique_dates)

for(i in 1:n_unique_dates){
  data_final.LNSF[[i]]  <- Winsorize(subset(data_final, Date==unique_dates[i], 
                                          select=c(LNSF)), na.rm = TRUE, probs = c(0.01, 0.99))
  
  ## add the date field to each winsorized data frame
  data_final.LNSF[[i]] <- cbind(data_final.LNSF[[i]], Date=unique_dates[i], REGN = REGN)
}

dLNSF <- bind_rows(data_final.LNSF)

dLNSF$REGN <- as.numeric(dLNSF$REGN)
dLNSF <- dLNSF[order(dLNSF[,2]),]


# WINSORIZE NPLF
## create an empty list of length Dates

data_final.NPLF <- vector("list", length=n_unique_dates)

for(i in 1:n_unique_dates){
  data_final.NPLF[[i]]  <- Winsorize(subset(data_final, Date==unique_dates[i], 
                                            select=c(NPLF)), na.rm = TRUE, probs = c(0.01, 0.99))
  
  ## add the date field to each winsorized data frame
  data_final.NPLF[[i]] <- cbind(data_final.NPLF[[i]], Date=unique_dates[i], REGN = REGN)
}

dNPLF <- bind_rows(data_final.NPLF)

dNPLF$REGN <- as.numeric(dNPLF$REGN)
dNPLF <- dNPLF[order(dNPLF[,2]),]


# WINSORIZE LNSH
## create an empty list of length Dates

data_final.LNSH <- vector("list", length=n_unique_dates)

for(i in 1:n_unique_dates){
  data_final.LNSH[[i]]  <- Winsorize(subset(data_final, Date==unique_dates[i], 
                                            select=c(LNSH)), na.rm = TRUE, probs = c(0.01, 0.99))
  
  ## add the date field to each winsorized data frame
  data_final.LNSH[[i]] <- cbind(data_final.LNSH[[i]], Date=unique_dates[i], REGN = REGN)
}

dLNSH <- bind_rows(data_final.LNSH)

dLNSH$REGN <- as.numeric(dLNSH$REGN)
dLNSH <- dLNSH[order(dLNSH[,2]),]


# WINSORIZE NPLH
## create an empty list of length Dates

data_final.NPLH <- vector("list", length=n_unique_dates)

for(i in 1:n_unique_dates){
  data_final.NPLH[[i]]  <- Winsorize(subset(data_final, Date==unique_dates[i], 
                                            select=c(NPLH)), na.rm = TRUE, probs = c(0.01, 0.99))
  
  ## add the date field to each winsorized data frame
  data_final.NPLH[[i]] <- cbind(data_final.NPLH[[i]], Date=unique_dates[i], REGN = REGN)
}

dNPLH <- bind_rows(data_final.NPLH)

dNPLH$REGN <- as.numeric(dNPLH$REGN)
dNPLH <- dNPLH[order(dNPLH[,2]),]


# WINSORIZE PR
## create an empty list of length Dates

data_final.PR <- vector("list", length=n_unique_dates)

for(i in 1:n_unique_dates){
  data_final.PR[[i]]  <- Winsorize(subset(data_final, Date==unique_dates[i], 
                                            select=c(PR)), na.rm = TRUE, probs = c(0.01, 0.99))
  
  ## add the date field to each winsorized data frame
  data_final.PR[[i]] <- cbind(data_final.PR[[i]], Date=unique_dates[i], REGN = REGN)
}

dPR <- bind_rows(data_final.PR)

dPR$REGN <- as.numeric(dPR$REGN)
dPR <- dPR[order(dPR[,2]),]


# WINSORIZE ROA
## create an empty list of length Dates

data_final.ROA <- vector("list", length=n_unique_dates)

for(i in 1:n_unique_dates){
  data_final.ROA[[i]]  <- Winsorize(subset(data_final, Date==unique_dates[i], 
                                           select=c(PR))/subset(data_final, Date==unique_dates[i], 
                                                                select=c(TA)), na.rm = TRUE, probs = c(0.01, 0.99))
  
  ## add the date field to each winsorized data frame
  data_final.ROA[[i]] <- cbind(data_final.ROA[[i]], Date=unique_dates[i], REGN = REGN)
}

dROA <- bind_rows(data_final.ROA)

dROA$REGN <- as.numeric(dROA$REGN)
dROA <- dROA[order(dROA[,2]),]


# MERGE WINSORIZED VARIABLES
data_win <- merge(dTA, dLNSF, by = c("Date", "REGN"), all = TRUE)
data_win <- merge(data_win, dNPLF, by = c("Date", "REGN"), all = TRUE)
data_win <- merge(data_win,  dLNSH, by = c("Date", "REGN"), all = TRUE)
data_win <- merge(data_win, dNPLH, by = c("Date", "REGN"), all = TRUE)
data_win <- merge(data_win, dPR, by = c("Date", "REGN"), all = TRUE)
data_win <- merge(data_win, dROA, by = c("Date", "REGN"), all = TRUE)

data_win <- data_win[order(data_win[,2]),]
data_win$Date <- as.Date(data_win$Date)
data_win <- data.frame(data_win)

data_final2 <- as.data.frame(data_win)
data_final2$Date <- as.Date(data_final2$Date)

colnames(data_final2) <- c("Date","REGN","TA","LNSF","NPLF","LNSH","NPLH","PR","ROA")


# ADD PLOTS FOR WINSORIZED DATA

# Nas to zeros if bank operates:
data_final4 <- data_final2
data_final4[is.na(data_final4) & data_final4$TA!=0] <- 0
data_final4 = subset(data_final4, data_final4$TA > 0)

# convert to a panel dataset:
data_final4 <- pdata.frame(data_final4,
                           index = c("REGN", "Date"),
                           row.names = TRUE)
data_final4$Date<-as.Date(as.character(data_final4$Date))

# add new variables:
data_final4$TAgrowth <- data_final4$TA/lag(data_final4$TA, 4)
data_final4$logTA <- log(data_final4$TA)


interval_size <- '3 month'
wanted_percentiles <- c(0.15, 0.25, 0.5, 0.75, 0.85)

get_quantiles <- function(items) {
  yy <- quantile(items, wanted_percentiles)
  return (yy)
}


r <- aggregate(logTA ~ cut(Date, interval_size), data_final4, get_quantiles)
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
  geom_rect(data=crisis08, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3) +
  geom_rect(data=crisis14, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3)+
  ggtitle("log(TA) percentiles in 3 months intervals (winsorizing)")


r <- aggregate(TAgrowth ~ cut(Date, interval_size), data_final4, get_quantiles)
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
  geom_rect(data=crisis08, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3) +
  geom_rect(data=crisis14, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3)+
  ggtitle("TA growth percentiles in 3 months intervals (winsorizing)")


r <- aggregate((TAgrowth-1)*100 ~ cut(Date, interval_size), data_final4, get_quantiles)
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
  labs(x = "Date", y = "TAgrowth, %") +
  geom_rect(data=crisis08, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3) +
  geom_rect(data=crisis14, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3)+
  ggtitle("TA growth percentiles in 3 months intervals (winsorizing)")


##### ADD VARIABLES + BEFORE/AFTER DESCRIPTIVE STATISTICS #####

# BEFORE: 
# NAs to zeros if TA>0 (Bank operates) + add new variables:
data_final4 <- data_final
data_final4[is.na(data_final4) & data_final4$TA!=0] <- 0
data_final4 = subset(data_final4, data_final4$TA > 0)
data_final4$logTA <- log(data_final4$TA)
data_final4$ROA<-(data_final4$PR/data_final4$TA)*100
data_final4$CLTA<-data_final4$LNSF/data_final4$TA
data_final4$RLTA<-data_final4$LNSH/data_final4$TA
data_final4$NPLs<-(data_final4$NPLH+data_final4$NPLF)/(data_final4$LNSF+data_final4$LNSH)

# convert to a panel dataset:
data_final4 <- pdata.frame(data_final4,
                           index = c("REGN", "Date"),
                           row.names = TRUE)

# add new variables:
data_final4$Date<-as.Date(as.character(data_final4$Date))
data_final4$TAgrowth <- (data_final4$TA/lag(data_final4$TA, k =4))

# report statistics:
stargazer(data_final4[ ,-1], type = "latex",
          digits=2, title="Table 1. Descriptive Statistics: before", out = "sum.tex")


# AFTER SIMPLE CLEANING:
# NAs to zeros if TA>0 (Bank operates)

# report statistics:
stargazer(data_final1[ ,-1], type = "latex",
          digits=2, title="Table 2. Descriptive Statistics: flat cleaning", out = "sum.tex")

# finalize dataset:
data_final_simple <- data_final1


# AFTER WINSORIZING:
# NAs to zeros if TA>0 (Bank operates):
data_final4 <- data_final2
data_final4[is.na(data_final4) & data_final4$TA!=0] <- 0
data_final4 = subset(data_final4, data_final4$TA > 0)

# convert to a panel dataset:
data_final4 <- pdata.frame(data_final4,
                           index = c("REGN", "Date"),
                           row.names = TRUE)
data_final4$Date<-as.Date(as.character(data_final4$Date))

# add new variables:
data_final4$TAgrowth <- data_final4$TA/lag(data_final4$TA, 4)
data_final4$logTA <- log(data_final4$TA)
data_final4$ROA<-data_final4$ROA*100
data_final4$CLTA<-data_final4$LNSF/data_final4$TA
data_final4$RLTA<-data_final4$LNSH/data_final4$TA
data_final4$NPLs<-(data_final4$NPLH+data_final4$NPLF)/(data_final4$LNSF+data_final4$LNSH)

# report statistics:
stargazer(data_final4[ ,-1], type = "latex",
          digits=2, title="Table 3. Descriptive Statistics: winsorizing data", out = "sum.tex")

# finalize dataset:
data_final_smart <- data_final4

# exclude NA-s from the final datasets:
data_final_simple <- na.exclude(data_final_simple)
data_final_smart <- na.exclude(data_final_smart)


##### TASK 2: REGRESSIONS (no ROA lags) #####

# ROBUST STANDART ERRORS:

clse = function(reg) { 
  # index(reg, "id") returns the id or entity variable vector 
  G = length(unique(index(reg,"id")))
  N = length(index(reg,"id"))
  dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
  rob = sqrt(diag(dfa*vcovHC(reg, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}



# SIMPLE (no ROA lags):

mod_1_simple <- plm(ROA ~CLTA+ RLTA+ NPLs+ logTA + TAgrowth
             , data = data_final_simple,index = c("REGN","Date"), model = "within", effect="twoways")


mod_2_simple <- plm(ROA ~CLTA+ RLTA+ NPLs
             , data = data_final_simple,index = c("REGN","Date"), model = "within", effect="twoways")


mod_3_simple <- plm(ROA ~CLTA+ RLTA+ NPLs+ logTA 
             , data = data_final_simple,index = c("REGN","Date"), model = "within", effect="twoways")


mod_4_simple <- plm(ROA ~CLTA+ RLTA+ NPLs + TAgrowth
             , data = data_final_simple,index = c("REGN","Date"), model = "within", effect="twoways")


# SMART (no ROA lags):

mod_1_smart <- plm(ROA ~CLTA+ RLTA+ NPLs+ logTA + TAgrowth
                   , data = data_final_smart,index = c("REGN","Date"), model = "within", effect="twoways")


mod_2_smart <- plm(ROA ~CLTA+ RLTA+ NPLs
             , data = data_final_smart,index = c("REGN","Date"), model = "within", effect="twoways")


mod_3_smart <- plm(ROA ~CLTA+ RLTA+ NPLs+ logTA 
             , data = data_final_smart,index = c("REGN","Date"), model = "within", effect="twoways")


mod_4_smart <- plm(ROA ~CLTA+ RLTA+ NPLs + TAgrowth
             , data = data_final_smart,index = c("REGN","Date"), model = "within", effect="twoways")


# WALD TESTS FOR SIMPLE AND SMART MODELS

w_simple<-waldtest(mod_2_simple, mod_1_simple, vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                   cluster = "group"))
w_simple[2, "Pr(>Chisq)"]

w_smart<-waldtest(mod_2_smart, mod_1_smart, vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                                         cluster = "group"))
w_smart[2, "Pr(>Chisq)"]



# RESULTS (no ROA lags)
stargazer(mod_1_simple, mod_1_smart, mod_2_simple, mod_2_smart, mod_3_simple, mod_3_smart, mod_4_simple, mod_4_smart,
          se=list(clse(mod_1_simple), clse(mod_1_smart), clse(mod_2_simple), clse(mod_2_smart), clse(mod_3_simple), clse(mod_3_smart), clse(mod_4_simple), clse(mod_4_smart)), 
          type="latex" , 
          title="Table 4. Flat cleaning vs. winsorizing data",
          column.labels=c("Both", "Both (win.)", "None", "None (win.)", "logTA", "logTA (win.)", "TAgrowth", "TAgrowth (win.)"), 
          column.sep.width = "2pt", row.sep.width = "2pt",font.size = "tiny",
          df=FALSE, digits=3)



# p-VALUES TABLE: 

sum_mod_1 <- summary(mod_1_smart,vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                           cluster = "group"))
sum_mod_2 <- summary(mod_2_smart,vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                           cluster = "group"))
sum_mod_3 <- summary(mod_3_smart,vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                           cluster = "group"))
sum_mod_4 <- summary(mod_4_smart,vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                           cluster = "group"))

sum_mod_1 <- summary(mod_1_simple,vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                           cluster = "group"))
sum_mod_2 <- summary(mod_2_simple,vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                           cluster = "group"))
sum_mod_3 <- summary(mod_3_simple,vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                           cluster = "group"))
sum_mod_4 <- summary(mod_4_simple,vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                           cluster = "group"))

pval <- matrix(c(w[2, "Pr(>Chisq)"],sum_mod_1[["coefficients"]][ , "Pr(>|t|)"]["logTA"],sum_mod_1[["coefficients"]][ , "Pr(>|t|)"]["TAgrowth"],
                 sum_mod_3[["coefficients"]][ , "Pr(>|t|)"]["logTA"],sum_mod_4[["coefficients"]][ , "Pr(>|t|)"]["TAgrowth"]),ncol=1,byrow=TRUE)
colnames(pval) <- c("p-value")
rownames(pval) <- c("joint","logTA only","TAgrowth only", "logTA", "TAgrowth")
pval <- as.table(pval)
stargazer(pval)


# ECONOMIC EFFECTS (FOR MODEL 1: HIGHER R2 ADJ.):

st_dev_ROA = sqrt(var(data_final_simple$ROA))
st_dev <-c(sqrt(var(data_final_simple$CLTA)),sqrt(var(data_final_simple$RLTA)),sqrt(var(data_final_simple$NPLs)),sqrt(var(data_final_simple$TAgrowth)))
clta_ef<-sqrt(var(data_final_simple$CLTA))*mod_1_simple$coefficients['CLTA']/st_dev_ROA
rlta_ef<-sqrt(var(data_final_simple$RLTA))*mod_1_simple$coefficients['RLTA']/st_dev_ROA
NPLs_ef<-sqrt(var(data_final_simple$NPLs))*mod_1_simple$coefficients['NPLs']/st_dev_ROA
TAgrowth_ef<-sqrt(var(data_final_simple$TAgrowth))*mod_1_simple$coefficients['TAgrowth']/st_dev_ROA

clta_ef
rlta_ef
NPLs_ef
TAgrowth_ef

st_dev_ROA = sqrt(var(data_final_smart$ROA))
st_dev <-c(sqrt(var(data_final_smart$CLTA)),sqrt(var(data_final_smart$RLTA)),sqrt(var(data_final_smart$NPLs)),sqrt(var(data_final_smart$TAgrowth)))
clta_ef<-sqrt(var(data_final_smart$CLTA))*mod_1_smart$coefficients['CLTA']/st_dev_ROA
rlta_ef<-sqrt(var(data_final_smart$RLTA))*mod_1_smart$coefficients['RLTA']/st_dev_ROA
NPLs_ef<-sqrt(var(data_final_smart$NPLs))*mod_1_smart$coefficients['NPLs']/st_dev_ROA
TAgrowth_ef<-sqrt(var(data_final_smart$TAgrowth))*mod_1_smart$coefficients['TAgrowth']/st_dev_ROA

clta_ef
rlta_ef
NPLs_ef
TAgrowth_ef

##### TASK 2: REGRESSIONS (ROA lags) #####


# SIMPLE (ROA lags):

mod_1_simple <- plm(ROA ~CLTA+ RLTA+ NPLs+ logTA + TAgrowth + lag(ROA)
                    , data = data_final_simple,index = c("REGN","Date"), model = "within", effect="twoways")


mod_2_simple <- plm(ROA ~CLTA+ RLTA+ NPLs + lag(ROA)
                    , data = data_final_simple,index = c("REGN","Date"), model = "within", effect="twoways")


mod_3_simple <- plm(ROA ~CLTA+ RLTA+ NPLs+ logTA + lag(ROA)
                    , data = data_final_simple,index = c("REGN","Date"), model = "within", effect="twoways")


mod_4_simple <- plm(ROA ~CLTA+ RLTA+ NPLs + TAgrowth + lag(ROA)
                    , data = data_final_simple,index = c("REGN","Date"), model = "within", effect="twoways")


# SMART (ROA lags):

mod_1_smart <- plm(ROA ~CLTA+ RLTA+ NPLs+ logTA + TAgrowth + lag(ROA)
                   , data = data_final_smart,index = c("REGN","Date"), model = "within", effect="twoways")


mod_2_smart <- plm(ROA ~CLTA+ RLTA+ NPLs + lag(ROA)
                   , data = data_final_smart,index = c("REGN","Date"), model = "within", effect="twoways")


mod_3_smart <- plm(ROA ~CLTA+ RLTA+ NPLs+ logTA + lag(ROA)
                   , data = data_final_smart,index = c("REGN","Date"), model = "within", effect="twoways")


mod_4_smart <- plm(ROA ~CLTA+ RLTA+ NPLs + TAgrowth + lag(ROA)
                   , data = data_final_smart,index = c("REGN","Date"), model = "within", effect="twoways")


# WALD TESTS FOR SIMPLE AND SMART MODELS

w_simple<-waldtest(mod_2_simple, mod_1_simple, vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                                         cluster = "group"))
w_simple[2, "Pr(>Chisq)"]

w_smart<-waldtest(mod_2_smart, mod_1_smart, vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                                      cluster = "group"))
w_smart[2, "Pr(>Chisq)"]




# RESULTS (ROA lags)
stargazer(mod_1_simple, mod_1_smart, mod_2_simple, mod_2_smart, mod_3_simple, mod_3_smart, mod_4_simple, mod_4_smart,
          se=list(clse(mod_1_simple), clse(mod_1_smart), clse(mod_2_simple), clse(mod_2_smart), clse(mod_3_simple), clse(mod_3_smart), clse(mod_4_simple), clse(mod_4_smart)), 
          type="latex" , 
          title="Table 5. Flat cleaning vs. winsorizing data (+ lag ROA)", 
          column.labels=c("Both", "Both (win.)", "None", "None (win.)", "logTA", "logTA (win.)", "TAgrowth", "TAgrowth (win.)"), 
          column.sep.width = "2pt", row.sep.width = "2pt",font.size = "tiny",
          df=FALSE, digits=3)


##### TASK 2: REGRESSIONS (BANK SIZE, ASSET GROWTH) #####


# BANK SIZE HYPOTHESIS:
mod_5_smart <- plm(ROA ~CLTA+ RLTA+ NPLs+ logTA +logTA*NPLs
             , data = data_final_smart,index = c("REGN","Date"), model = "within", effect="twoways")

sum_mod_5_smart<- summary(mod_5_smart, vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                     cluster = "group"))
sum_mod_5_smart


# ASSETS GROWTH HYPOTHESIS:
data_final_smart$TAgrowth_2 <-data_final_smart$TAgrowth^2

mod_6_smart <- plm(ROA ~CLTA+ RLTA+ NPLs+ TAgrowth +TAgrowth_2
             , data = data_final_smart,index = c("REGN","Date"), model = "within", effect="twoways")

sum_mod_6_smart <- summary(mod_6_smart, vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                     cluster = "group"))
sum_mod_6_smart


# BANK SIZE HYPOTHESIS (+ lag ROA):
mod_7_smart <- plm(ROA ~CLTA+ RLTA+ NPLs+ logTA +logTA*NPLs+lag(ROA, 1)
             , data = data_final_smart,index = c("REGN","Date"), model = "within", effect="twoways")

sum_mod_7_smart <- summary(mod_7_smart, vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                     cluster = "group"))
sum_mod_7_smart


# ASSETS GROWTH HYPOTHESIS (+ lag ROA):
mod_8_smart <- plm(ROA ~CLTA+ RLTA+ NPLs+ TAgrowth +TAgrowth_2+lag(ROA, 1)
                   , data = data_final_smart,index = c("REGN","Date"), model = "within", effect="twoways")

sum_mod_8_smart <- summary(mod_8_smart, vcov = function(x) vcovHC(x, method = "arellano",type = "HC1", 
                                                                  cluster = "group"))
sum_mod_8_smart


# RESULTS (BANK SIZE, ASSET GROWTH)
stargazer(mod_5_smart, mod_7_smart, mod_6_smart, mod_8_smart,
          se=list(clse(mod_5_smart), clse(mod_7_smart), clse(mod_6_smart), clse(mod_8_smart)), 
          type="latex" , 
          title="Table 6. Bank size and Assets growth hypothesis",
          column.labels=c("Size Hyp.", "Size Hyp. + lag", "Growth Hyp.", "Growth Hyp. + lag"), 
          column.sep.width = "2pt", row.sep.width = "2pt",font.size = "tiny",
          df=FALSE, digits=3)



##### TASK 3 (SFA: OC vs. CAOC) #####

library(tidyverse)
library(haven)

##### TASK 3: DATA MANIPULATION #####

data <- read_dta('PanelRusBanks.dta') 

# Remove negative values:

data <- data %>% filter(TC >= 0 & RevalsNeg >= 0 & IE >= 0 & 
                          PE >= 0 & FEE >= 0 & TA >= 0)

# NAs to zeros if bank operates:

data$IE[!is.na(data$TA)] <-  replace_na(data$IE[!is.na(data$TA)], 0)
data$RevalsPos[!is.na(data$TA)] <-  replace_na(data$RevalsPos[!is.na(data$TA)], 0)
data$RevalsNeg[!is.na(data$TA)] <-  replace_na(data$RevalsNeg[!is.na(data$TA)], 0)
data$PE[!is.na(data$TA)] <-  replace_na(data$PE[!is.na(data$TA)], 0)
data$FEE[!is.na(data$TA)] <-  replace_na(data$FEE[!is.na(data$TA)], 0)
data$LF[!is.na(data$TA)] <-  replace_na(data$LF[!is.na(data$TA)], 0)
data$LH[!is.na(data$TA)] <-  replace_na(data$LH[!is.na(data$TA)], 0)
data$DF[!is.na(data$TA)] <-  replace_na(data$DF[!is.na(data$TA)], 0)
data$DH[!is.na(data$TA)] <-  replace_na(data$DH[!is.na(data$TA)], 0)
data$LA[!is.na(data$TA)] <-  replace_na(data$LA[!is.na(data$TA)], 0)
data$LF3[!is.na(data$TA)] <-  replace_na(data$LF3[!is.na(data$TA)], 0)
data$NPLF[!is.na(data$TA)] <-  replace_na(data$NPLF[!is.na(data$TA)], 0)
data$NPLH[!is.na(data$TA)] <-  replace_na(data$NPLH[!is.na(data$TA)], 0)
data$IBL[!is.na(data$TA)] <-  replace_na(data$IBL[!is.na(data$TA)], 0)
data$IBD[!is.na(data$TA)] <-  replace_na(data$IBD[!is.na(data$TA)], 0)
data$FA[!is.na(data$TA)] <-  replace_na(data$FA[!is.na(data$TA)], 0)
data$FL[!is.na(data$TA)] <-  replace_na(data$FL[!is.na(data$TA)], 0)
data$EQ[!is.na(data$TA)] <-  replace_na(data$EQ[!is.na(data$TA)], 0)

# FLAT CLEANING:

# Get boolean dataset of non-outliers:
keep <- Reduce(`&`, lapply(data[, c(5:38)], function(x) x >= quantile(x, .01, na.rm = TRUE) 
                           & x <= quantile(x, .99, na.rm = TRUE)))
# Keep non-outliers:
data <- data[keep,]

#Y-s: BANK OUTPUTS (FEEs included)

data$LNS = data$LF+data$LH+data$IBL+data$LF3 + data$LH3 
data$DEP = data$DF+data$DH+data$IBD+data$DH3 

#P-s: BANK INPUT PRICES

data$AFR <- data$IE/data$TA*100 
data$W <- data$PE/data$TA*100  
data$R <- (data$TC- data$IE - data$PE - data$RevalsNeg)/data$TA*100 

# BANK NETPUTS (EQ included)

# TIME TRENDS

data$T <- data$quar_id
data$T_sq <- (data$T)^2

# EXPLANATORY VARIABLES FOR INEFFICIENCY:

data$log_TA     = log(data$TA+1) 
data$LiqTA    = data$LA/data$TA*100	 
data$LF3TA    = data$LF3/data$TA*100	
data$LH3TA    = data$LH3/data$TA*100		 	
data$LTA      = (data$LF+data$LH)/data$TA*100 
data$NPL      = (data$NPLF+data$NPLH)/(data$LF+data$LH)*100	
data$EqTA     = data$EQ/data$TA*100		

# LOG-s: 

data$log_LNS <- log(data$LNS +1)
data$log_DEP <- log(data$DEP +1)
data$log_FEE <- log(data$FEE + 1)
data$log_AFR <- log(data$AFR +1)
data$log_R <- log(data$R+1)
data$log_W <- log(data$W + 1)
data$log_EQ <- log(data$EQ + 1)


# CROSS-PRODUCTS:

data$log_AFR_sq = (data$log_AFR)^2
data$log_AFR_log_W = data$log_AFR*data$log_W
data$log_AFR_log_R = data$log_AFR*data$log_R
data$log_R_sq = data$log_R^2
data$log_R_log_W = data$log_R*data$log_W
data$log_W_sq = (data$log_W)^2
data$log_AFR_cub = (data$log_AFR)^3
data$log_R_cub = (data$log_R)^3
data$log_W_cub = (data$log_W)^3
data$log_AFR_sq_log_W = (data$log_AFR)^2*data$log_W
data$log_AFR_log_W_sq = data$log_AFR*(data$log_W)^2
data$log_AFR_sq_log_R = (data$log_AFR)^2*data$log_R
data$log_AFR_log_R_sq = data$log_AFR*(data$log_R)^2
data$log_R_sq_log_W = data$log_R^2*data$log_W
data$log_R_log_W_sq = data$log_R*(data$log_W)^2

data$log_AFR_log_LNS = data$log_AFR*data$log_LNS
data$log_W_log_LNS = data$log_W*data$log_LNS
data$log_R_log_LNS = data$log_R*data$log_LNS

data$log_AFR_log_DEP = data$log_AFR*data$log_DEP
data$log_W_log_DEP = data$log_W*data$log_DEP
data$log_R_log_DEP = data$log_R*data$log_DEP

data$log_AFR_log_FEE = data$log_AFR*data$log_FEE
data$log_W_log_FEE = data$log_W*data$log_FEE
data$log_R_log_FEE = data$log_R*data$log_FEE

data$log_AFR_log_EQ = data$log_AFR*data$log_EQ
data$log_W_log_EQ = data$log_W*data$log_EQ
data$log_R_log_EQ = data$log_R*data$log_EQ

data$T_log_AFR = data$log_AFR*data$T
data$T_log_W = data$log_W*data$T
data$T_log_R = data$log_R*data$T

data$log_LNS_sq = (data$log_LNS)^2
data$log_LNS_log_DEP = data$log_LNS*data$log_DEP
data$log_LNS_log_FEE = data$log_LNS*data$log_FEE
data$log_DEP_sq = (data$log_DEP)^2
data$log_DEP_log_FEE = data$log_DEP*data$log_FEE
data$log_FEE_sq = (data$log_FEE)^2

data$log_EQ_sq = (data$log_EQ)^2
data$log_EQ_log_LNS = data$log_EQ*data$log_LNS
data$log_EQ_log_DEP = data$log_EQ*data$log_DEP
data$log_EQ_log_FEE = data$log_EQ*data$log_FEE

data$T_log_LNS=data$T*data$log_LNS
data$T_log_DEP=data$T*data$log_DEP
data$T_log_FEE=data$T*data$log_FEE
data$T_log_EQ=data$T*data$log_EQ

# DEPENDENT VARIABLE:

data$OC <- data$TC-data$IE
data$log_OC <- log(data$OC + 1) 
data$log_OC_adj = log(data$TC-data$IE-data$RevalsPos) 

# FXLOANS, NER, CROSS-PRODUCTS (FOR SFA - ESTIMATED OCadj.):

data$fxloans <- data$fxloans_to_totalloans*data$LNS/100 
data$log_fxloans <- log(data$fxloans + 1)  
data$fxloans_ner <- data$fxloans * data$ner
data$fxloans_ner_12m <- data$fxloans * data$ner_12m

data$log_ner <- log(data$ner+1) 
data$log_ner_sq <- log(data$log_ner)^2
data$log_LNS_log_ner <- data$log_LNS*data$log_ner
data$log_DEP_log_ner <- data$log_DEP*data$log_ner
data$log_FEE_log_ner <- data$log_FEE*data$log_ner
data$log_AFR_log_ner <-  data$log_AFR*data$log_ner
data$log_R_log_ner <-  data$log_R*data$log_ner
data$log_W_log_ner <- data$log_W*data$log_ner
data$log_EQ_log_ner <- data$log_EQ*data$log_ner


##### TASK 3: OC REGRESSION (OC_adj NO REVALS) #####

data <- pdata.frame(data,index = c("REGN", "QUAR"),row.names = TRUE)

# ADD VARIABLES WITH LAGS:

data$ner_lag <- lag(data$ner, 1)
data$ner_lag2 <- lag(data$ner, 2)
data$GrowTA4Q = (data$TA/lag(data$TA, 4)-1)*100	
data$ner <- as.numeric(data$ner)
data$ner_lag <- as.numeric(data$ner_lag)
data$ner_lag2 <- as.numeric(data$ner_lag2)




# GET ESTIMATED OCadj-s:

model <- plm(log_OC ~ log(ner) + log(ner_lag) + log(ner_lag2) + LiqTA + LF3TA + LH3TA +
            + LTA + NPL + EqTA + GrowTA4Q, data = data,
            index = c("REGN","QUAR"), model = "within", effect="individual")

coefs <- model$coefficients[1:3]

data$log_hat_oc <- data$log_OC-coefs[1]*log(data$ner)-coefs[2]*log(data$ner_lag)-coefs[3]*log(data$ner_lag2)


##### TASK 3: SFA (NO RESTRICTIONS) #####

# FIRST STAGE VARIABLES (BASELINE):

x <- c('log_AFR', 'log_W', 'log_R', 'log_AFR_sq','log_W_sq', 'log_R_sq',
       'log_AFR_log_W',  'log_AFR_log_R', 'log_R_log_W', 'log_AFR_log_LNS',
       'log_W_log_LNS', 'log_R_log_LNS', 'log_AFR_log_DEP', 'log_W_log_DEP',
       'log_R_log_DEP', 'log_AFR_log_FEE', 'log_W_log_FEE','log_R_log_FEE',
       'log_AFR_log_EQ', 'log_W_log_EQ', 'log_R_log_EQ', 'T', 'T_sq', 'log_EQ', 
       'log_EQ_sq', 'log_EQ_log_LNS', 'log_EQ_log_DEP', 'log_EQ_log_FEE', 'T_log_EQ',
       'T_log_AFR', 'T_log_W', 'T_log_R', 'T_log_LNS', 'T_log_DEP', 'T_log_FEE',
       'log_LNS', 'log_LNS_sq', 'log_DEP', 'log_DEP_sq',
       'log_FEE', 'log_FEE_sq', 'log_LNS_log_DEP', 'log_LNS_log_FEE', 'log_DEP_log_FEE')

# SECOND STAGE VARIABLES (BASELINE):

z <-  c('BigFour', 'OtherStateBanks', 'Foreign', 'SANAT', 'TightPrudRegime', 
        'LiqTA', 'LF3TA', 'LH3TA', 'GrowTA4Q', 'EqTA')

# SFA FOR OC (BASELINE):

sfa_reg_OC <- frontier('log_OC',
                       xNames = x,
                       zNames = z, 
                       zIntercept =  FALSE, data = data,  ineffDecrease = FALSE)

summary1 <- summary(sfa_reg_OC) 

# SFA FOR OCadj (REVALS USED):

sfa_reg_OC_adj <- frontier('log_OC_adj',
                           xNames = x, 
                           zNames = z, 
                           zIntercept =  FALSE, data = data,  ineffDecrease = FALSE)

summary2 <- summary(sfa_reg_OC_adj) 


# SFA FOR OCadj (NO REVALS USED):

# ADD VARIABLES TO 1eq. (with NER):

x1 <- c('log_AFR', 'log_W', 'log_R', 'log_AFR_sq','log_W_sq', 'log_R_sq',
        'log_AFR_log_W',  'log_AFR_log_R', 'log_R_log_W', 'log_AFR_log_LNS',
        'log_W_log_LNS', 'log_R_log_LNS', 'log_AFR_log_DEP', 'log_W_log_DEP',
        'log_R_log_DEP', 'log_AFR_log_FEE', 'log_W_log_FEE','log_R_log_FEE',
        'log_AFR_log_EQ', 'log_W_log_EQ', 'log_R_log_EQ', 'T', 'T_sq', 'log_EQ', 
        'log_EQ_sq', 'log_EQ_log_LNS', 'log_EQ_log_DEP', 'log_EQ_log_FEE', 'T_log_EQ',
        'T_log_AFR', 'T_log_W', 'T_log_R', 'T_log_LNS', 'T_log_DEP', 'T_log_FEE',
        'log_LNS', 'log_LNS_sq', 'log_DEP', 'log_DEP_sq',
        'log_FEE', 'log_FEE_sq', 'log_LNS_log_DEP', 'log_LNS_log_FEE', 'log_DEP_log_FEE', 'log_ner', 'log_ner_sq',
        'log_LNS_log_ner', 'log_DEP_log_ner',
        'log_FEE_log_ner', 'log_AFR_log_ner',
        'log_R_log_ner', 'log_W_log_ner')

# ADD VARIABLES TO 2 eq. (FXLOANS, NER):

z1 <-  c('BigFour', 'OtherStateBanks', 'Foreign', 'SANAT', 'TightPrudRegime', 
         'LiqTA', 'LF3TA', 'LH3TA', 'GrowTA4Q', 'EqTA', 'fxloans', 'ner_12m', 'fxloans_ner_12m')

sfa_reg_hat_OC <- frontier('log_hat_oc',
                           xNames = x1, 
                           zNames = z1, 
                           zIntercept =  FALSE, data = data,  ineffDecrease = FALSE)

summary3 <- summary(sfa_reg_hat_OC) 


# PLOT MEAN EFFICIENCY FOR EACH YEAR: 

df <- summary1$efficYearMeans
df <- as.data.frame(df)
df$OC <- summary1$efficYearMeans
df$ts <- seq(as.Date("2008-01-01"), by= "3 month", length.out= 47)
df$OC_adj <- summary2$efficYearMeans
df$hat_OC <- summary3$efficYearMeans

ggplot(df, aes(x = ts)) +
  
  geom_line(aes(y = `OC`, col="OC")) +
  geom_line(aes(y = `OC_adj`, col="CAOC (Revals)")) +
  geom_line(aes(y = `hat_OC`, col="CAOC (No Revals)")) +
  
  labs(x = "Date", y = "Mean Efficiency") +
  geom_rect(data=crisis08, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3) +
  geom_rect(data=crisis14, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3)+
  ggtitle("Mean efficiency for each year (no restrictions, flat cleaning)")


##### EFICIENCY PERCENTILES #####

# MAKE PANEL DATASET OF ESTIMATED EFFICIENCIES:

ef1 <- efficiencies(summary3)
ef1 <- cbind(rownames(ef1), data.frame(ef1, row.names=NULL))

colnames(ef1) <- c("REGN", paste("ef", df$ts, sep = "_"))
ef1$REGN <- as.numeric(ef1$REGN)

ef1 <- reshape(ef1, 
                direction = "long",
                varying = list(names(ef1)[2:48]),
                v.names = "efficiency",
                idvar = c("REGN"),
                timevar = "Date",
                times = df$ts)
rownames(ef1) <- NULL


# GET PLOTS (MEANS AND PERCENTILES):

interval_size <- '3 month'
wanted_percentiles <- c(0.15, 0.25, 0.5, 0.75, 0.85)

get_quantiles <- function(items) {
  yy <- quantile(items, wanted_percentiles, na.rm = TRUE)
  return (yy)
}

r <- aggregate(efficiency ~ cut(Date, interval_size), ef1, get_quantiles)
names(r) <- c("ts", "t")
df <- as.data.frame(r$t)
df$ts <- r$ts
df$ts <- as.Date(df$ts)

crisis08 <- data.frame(xmin=as.Date('2008-07-01'), xmax=as.Date('2010-07-01'), ymin=-Inf, ymax=Inf)
crisis14 <- data.frame(xmin=as.Date('2014-01-01'), xmax=as.Date('2015-07-01'), ymin=-Inf, ymax=Inf)

ggplot(df, aes(x = ts)) +
  geom_line(aes(y = `15%`, col="15%")) +
  geom_line(aes(y = `25%`, col="25%")) +
  geom_line(aes(y = `50%`, col="50%")) +
  geom_line(aes(y = `75%`, col="75%")) +
  geom_line(aes(y = `85%`, col="85%")) +
  labs(x = "Date", y = "Efficiency") +
  geom_rect(data=crisis08, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3) +
  geom_rect(data=crisis14, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes = FALSE, color="transparent", alpha=0.3)+
  ggtitle("Efficiency percentiles (flat cleaning, OC estimated)")


# GET RESULTS TABLE FOR SFA:

stargazer(coef(summary3), digits = 2)

# GET RESULTS TABLE FOR lnOC_hat REGRESSION:

stargazer(model, digits = 3)


##### DIFFERENTIAL EQUATION (LONG STORY) #####


install.packages("rmutil")
library(rmutil)

fn <- function(y,x)(y - y^2)
soln <- runge.kutta(fn, 0.001, seq(0.1,20,by=0.1))
plot(seq(0.01,20,by=0.1), soln)

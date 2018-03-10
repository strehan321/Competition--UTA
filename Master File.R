###################################################################
# Team - Maverick
####################################################################

# Installing libraries
install.packages("xlsx")
install.packages("rJava")
library(xlsx)
library(rJava)

# Clearing environment
rm(list=ls(all=TRUE))

# Reading necessary files
sales<-read.csv("Sales.txt", sep = "|", header = TRUE, stringsAsFactors = FALSE)
traffic<-read.csv("traffic.txt", sep = "", header = TRUE, stringsAsFactors = FALSE)
productfeature <- read.csv("C:/Users/Shaurya/Desktop/UTA_Competition_2018/UTA_Competition_2018/FeaturedProducts.csv")
Product<-read.csv("C:/Users/Shaurya/Desktop/UTA_Competition_2018/UTA_Competition_2018/ProductDataSet/UTA_ProductDataSet.csv", sep = "|", header = TRUE, stringsAsFactors = FALSE)

# Merging sales and traffic data
colnames(sales)
colnames(traffic)
d1 = merge(sales,traffic,by.x = "ActivityDate", by.y = "TrafficDate")


# Handeling Date 
productfeature$Start.Date<-as.Date(productfeature$Start.Date,format = "%m/%d/%Y")
productfeature$End.Date<-as.Date(productfeature$End.Date,format = "%m/%d/%Y")
d1$ActivityDate<-as.Date(d1$ActivityDate,"%Y-%m-%d")

# Merging Sales, Traffic and Product Feature file
colnames(d1)
colnames(productfeature)
d2 = merge(d1,productfeature,by.x = "SKU", by.y ="Sku")

# head(productfeature$Start.Date)
# str(productfeature$Start.Date)
# class(productfeature$End.Date)
class(d2$ActivityDate)

# Subsetting on the basis of Start Date =< Activity Date =< End Date
d3 <- d2[which((d2$ActivityDate >= d2$Start.Date) & (d2$ActivityDate <= d2$End.Date)),]

# Data Cleaning

Product <- Product[,c(1,3:5,11:13)]
names(Product)[1]<-"SKU"

# Creating new columns for analysis
str(d3$Code)
d3$EM <- d3$Code=="EM"
d3$EM[d3$EM =="TRUE"] <- 1
d3$EM[d3$EM =="FALSE"] <- 0

d3$OB <- d3$Code=="OB"
d3$OB[d3$OB =="TRUE"] <- 1
d3$OB[d3$OB =="FALSE"] <- 0

d3$OP1 <- d3$Code=="OP1"
d3$OP1[d3$OP1 =="TRUE"] <- 1
d3$OP1[d3$OP1 =="FALSE"] <- 0

d3$OSM <- d3$Code=="OSM"
d3$OSM[d3$OSM =="TRUE"] <- 1
d3$OSM[d3$OSM =="FALSE"] <- 0

str(Product$ChannelStrategy)

Product$Normal <- Product$ChannelStrategy=="Normal"
Product$Normal[Product$Normal =="TRUE"] <- 1
Product$Normal[Product$Normal =="FALSE"] <- 0

Product$U2 <- Product$ChannelStrategy=="2U"
Product$U2[Product$U2 =="TRUE"] <- 1
Product$U2[Product$U2 =="FALSE"] <- 0

Product$XR <- Product$ChannelStrategy=="XR"
Product$XR[Product$XR =="TRUE"] <- 1
Product$XR[Product$XR =="FALSE"] <- 0

str(Product$SKUPurchaseType)

Product$Normal1 <- Product$SKUPurchaseType=="Normal"
Product$Normal1[Product$Normal1 =="TRUE"] <- 1
Product$Normal1[Product$Normal1 =="FALSE"] <- 0

Product$Rebuy <- Product$SKUPurchaseType=="Rebuy"
Product$Rebuy[Product$Rebuy =="TRUE"] <- 1
Product$Rebuy[Product$Rebuy =="FALSE"] <- 0

Product$NotForSale <- Product$SKUPurchaseType=="Not For Sale"
Product$NotForSale[Product$NotForSale =="TRUE"] <- 1
Product$NotForSale[Product$NotForSale =="FALSE"] <- 0

# Master File - Merging Product & all other data
colnames(d3)
colnames(Product)
d4 = merge(d3,Product,by.x = c("SKU","Dept","Class"), by.y = c("SKU","Department","Class"))
colnames(d4)
# write.csv(d4,"d4.csv")
d4 <- read.csv("d4.csv")

# Descriptive Statistics 
d4_vehicle <- d4[,17:20] 
d4_channel <- d4[,25:27]
d4_sale    <- d4[,28:30]

# install.packages("corrgram")
# install.packages("corrplot")

# library(corrgram)
library(corrplot)

# correlation plot between vehicles 

corrplot(cor(d4_channel), method = "circle", type = "upper")
corrplot(cor(d4_vehicle), method = "circle", type = "upper")
corrplot(cor(d4_vehicle), method = "circle", type = "upper")

corrplot(cor(d4_channel), method = "pie", type = "upper")
corrplot(cor(d4_vehicle), method = "pie", type = "upper")
corrplot(cor(d4_vehicle), method = "pie", type = "upper")

corrplot(cor(d4_channel), method = "number", type = "upper")
corrplot(cor(d4_vehicle), method = "number", type = "upper")
corrplot(cor(d4_vehicle), method = "number", type = "upper")

colnames(d4)
#Decomposing the sales into a trend, a seasonal and an irregular component; 
#We observe a yearly seasonal trend should not effect our final modelling parameters

max(d4$Start.Date)
# [1] "2017-11-30"

min(d4$Start.Date)
# [1] "2016-01-01

################################################################################################################

# Modelling for forecasting

# Step 1 : Loading R packages
library(ggplot2)
library(TTR)
library(forecast)
library(tseries)

# Taking only Instore Sales Quantity for further analysis

# Step 2 : Examining data
ts_sales_instore<- ts(d4$InStoreSalesQuantity, frequency=52, start = c(2016,1), end = c(2018,1)) 
plot.ts((ts_sales_instore)) #Plot the complete signal
ggplot(d4, aes(ActivityDate, d4$InStoreSalesQuantity)) + geom_line() + scale_x_date('month')  + ylab("Instore Sales") +
  xlab("")

# Step 3 : Handling Missing Values and cleaning data
ts_sales_instore <- tsclean(ts_sales_instore)
plot.ts((ts_sales_instore)) #Plot the complete signal
ggplot(d4, aes(ActivityDate, d4$InStoreSalesQuantity)) + geom_line() + scale_x_date('month')  + ylab("Instore Sales") +
  xlab("")

# Step 4 : Smoothing Noise Fluctuations
# Draw a line through the series tracing its bigger troughs and peaks while smoothing out noisy fluctuations

# Daily Moving Averages
d4$InStoreSalesQuantity = ma(d4$InStoreSalesQuantity, order=1)

# Weekly Moving Averages
d4$InStoreSalesQuantity_week = ma(d4$InStoreSalesQuantity, order=7) 

# Monthly Moving Averages
d4$InStoreSalesQuantity_month = ma(d4$InStoreSalesQuantity, order=30)

ggplot() +
  geom_line(data = d4, aes(x = ActivityDate, y = InStoreSalesQuantity, colour = "Counts")) +
  geom_line(data = d4, aes(x = ActivityDate, y = InStoreSalesQuantity_week,   colour = "Weekly Moving Average"))  +
  geom_line(data = d4, aes(x = ActivityDate, y = InStoreSalesQuantity_month, colour = "Monthly Moving Average"))  +
  ylab('Instore Sales Quantity')



# Step 5 : Decomposing signal using fourier transformation
InStoreSalesQuantity_month = ts(na.omit(d4$InStoreSalesQuantity_month), frequency=30,start = c(2016,1), end = c(2018,1))
decomp = stl(InStoreSalesQuantity_month, s.window="periodic")
deseasonal_month <- seasadj(decomp)
plot(decomp)

# Step 6 : Check for Stationarity
adf.test(InStoreSalesQuantity_month, alternative = "stationary")
# 
# Augmented Dickey-Fuller Test
# 
# data:  InStoreSalesQuantity_month
# Dickey-Fuller = -2.3127, Lag order = 3, p-value = 0.4484
# alternative hypothesis: stationary

Acf(InStoreSalesQuantity_month, main='')
Pacf(InStoreSalesQuantity_month, main='')


count_d1 = diff(deseasonal_month, differences = 2)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

# Augmented Dickey-Fuller Test
# 
# data:  count_d1
# Dickey-Fuller = -3.5997, Lag order = 3, p-value = 0.04092
# alternative hypothesis: stationary

# This suggests that differences of 2 is required with 95% of Confidence Interval

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')


# Step 7 : Fitting ARIMA Model
auto.arima(deseasonal_month, seasonal=FALSE)
# 
# Series: deseasonal_month 
# ARIMA(1,2,0)                    
# 
# Coefficients:
#       ar1
#       0.4673
# s.e.  0.1218
# 
# sigma^2 estimated as 0.03457:  log likelihood=15.98
# AIC=-27.97   AICc=-27.75   BIC=-23.81


# Step 8 : Evaluation & Iterate

fit<-auto.arima(deseasonal_month, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,2,0) Model Residuals')

# Selecting AR = 1 ,  DL = 2 , MA = 3 based upon lowest AIC value
fit2 = arima(deseasonal_month, order=c(1,2,3))
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model (1,2,3) Model Residuals')

# Step 9 : Forecasting
fcast <- forecast(fit2, h=30)
plot(fcast)

write.csv(fcast,"fcast.csv")

###############################################################################################################################


# Modelling for forecasting - Online

# Step 1 : Loading R packages
library(ggplot2)
library(TTR)
library(forecast)
library(tseries)

# Taking only Instore Sales Quantity for further analysis

# Step 2 : Examining data
ts_sales_online<- ts(d4$OnlineSalesQuantity, frequency=52, start = c(2016,1), end = c(2018,1)) 
plot.ts((ts_sales_online)) #Plot the complete signal
ggplot(d4, aes(ActivityDate, d4$OnlineSalesQuantity)) + geom_line() + scale_x_date('month')  + ylab("Online") +
  xlab("")

# Step 3 : Handling Missing Values and cleaning data
ts_sales_online <- tsclean(ts_sales_online)
plot.ts((ts_sales_online)) #Plot the complete signal
ggplot(d4, aes(ActivityDate, d4$OnlineSalesQuantity)) + geom_line() + scale_x_date('month')  + ylab("Instore Sales") +
  xlab("")

# Step 4 : Smoothing Noise Fluctuations
# Draw a line through the series tracing its bigger troughs and peaks while smoothing out noisy fluctuations

# Daily Moving Averages
d4$OnlineSalesQuantity = ma(d4$OnlineSalesQuantity, order=1)

# Weekly Moving Averages
d4$OnlineSalesQuantity_week = ma(d4$OnlineSalesQuantity, order=7) 

# Monthly Moving Averages
d4$OnlineSalesQuantity_month = ma(d4$OnlineSalesQuantity, order=30)

ggplot() +
  geom_line(data = d4, aes(x = ActivityDate, y = OnlineSalesQuantity, colour = "Counts")) +
  geom_line(data = d4, aes(x = ActivityDate, y = OnlineSalesQuantity_week,   colour = "Weekly Moving Average"))  +
  geom_line(data = d4, aes(x = ActivityDate, y = OnlineSalesQuantity_month, colour = "Monthly Moving Average"))  +
  ylab('Online Sales Quantity')

# Step 5 : Decomposing signal using fourier transformation
OnlineSalesQuantity_month = ts(na.omit(d4$OnlineSalesQuantity_month), frequency=30,start = c(2016,1), end = c(2018,1))
decomp = stl(OnlineSalesQuantity_month, s.window="periodic")
deseasonal_month1 <- seasadj(decomp)
plot(decomp)

# ts_sales_components_instore <- decompose(d4$OnlineSalesQuantity)
# 
# # Plotting Seasonal, Trend & Random behavior = Observed Signal
# plot(ts_sales_components_instore, xlab="time (2 years)",cex=5, frame.plot=TRUE,col.axis="grey",col=c("blue"),axes=TRUE)
# 
# # Removing seasonal component from the signal
# seasonally_adjusted_sales_instore <- OnlineSalesQuantity - ts_sales_components_instore$seasonal
# plot(seasonally_adjusted_sales_instore) #the signal minus the seasonal componenet


# Step 6 : Check for Stationarity
adf.test(OnlineSalesQuantity_month, alternative = "stationary")
# 
# Augmented Dickey-Fuller Test
# 
# data:  OnlineSalesQuantity_month
# Dickey-Fuller = -2.9958, Lag order = 3, p-value = 0.1721
# alternative hypothesis: stationary

Acf(OnlineSalesQuantity_month, main='')
Pacf(OnlineSalesQuantity_month, main='')


count_d2 = diff(deseasonal_month1, differences = 2)
plot(count_d2)
adf.test(count_d2, alternative = "stationary")


# Augmented Dickey-Fuller Test
# 
# data:  count_d2
# Dickey-Fuller = -4.2626, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

# This suggests that differences of 2 is required with 99% of Confidence Interval

Acf(count_d2, main='ACF for Differenced Series')
Pacf(count_d2, main='PACF for Differenced Series')


# Step 7 : Fitting ARIMA Model
auto.arima(deseasonal_month1, seasonal=FALSE)
# 
# Series: deseasonal_month 
# ARIMA(1,2,0)                    
# 
# Coefficients:
#       ar1
#       0.4673
# s.e.  0.1218
# 
# sigma^2 estimated as 0.03457:  log likelihood=15.98
# AIC=-27.97   AICc=-27.75   BIC=-23.81


# Step 8 : Evaluation & Iterate

fit1<-auto.arima(deseasonal_month1, seasonal=FALSE)
tsdisplay(residuals(fit1), lag.max=45, main='(1,2,2) Model Residuals')

# Selecting AR = 1 ,  DL = 2 , MA = 3 based upon lowest AIC value
fit21 = arima(deseasonal_month1, order=c(1,2,2))
fit21
tsdisplay(residuals(fit21), lag.max=15, main='Seasonal Model Residuals')

# Step 9 : Forecasting
fcast1 <- forecast(fit21, h=30)
plot(fcast1)

# ###############
# # Dividing data into components for visualization
# 
library("TTR") #Needed for time series
ts_sales1<- ts(d4$InStoreSalesQuantity, frequency=52,start=c(2016,1)) #Assuming the data is for the last 2 years; Doesnt change the outcome of the analysis
ts_sales2<- ts(d4$OnlineSalesQuantity, frequency=52,start=c(2016,1)) #Assuming the data is for the last 2 years; Doesnt change the outcome of the analysis
plot.ts((ts_sales1)) #Plot the complete signal
plot.ts((ts_sales2))
ts_sales_components1 <- decompose(ts_sales1) #Sort of a fourier transformation to decompose signal
ts_sales_components2 <- decompose(ts_sales2)
plot(ts_sales_components1, xlab="time (2 years)",cex=5, frame.plot=TRUE,col.axis="grey",col=c("blue"),axes=FALSE)
plot(ts_sales_components2, xlab="time (2 years)",cex=5, frame.plot=TRUE,col.axis="grey",col=c("blue"),axes=FALSE)

seasonally_adjusted_sales1 <- ts_sales1 - ts_sales_components1$seasonal
seasonally_adjusted_sales2 <- ts_sales2 - ts_sales_components2$seasonal

plot(seasonally_adjusted_sales1) #the signal minus the seasonal componenet
plot(seasonally_adjusted_sales2)

# 
# colnames(d4)

write.csv(d4,"t.csv")
d4 <- read.csv('d4.csv')

# 
# ts_sales_components_instore <- decompose(ts_sales_instore)
# 
# # Plotting Seasonal, Trend & Random behavior = Observed Signal
# plot(ts_sales_components_instore, xlab="time (2 years)",cex=5, frame.plot=TRUE,col.axis="grey",col=c("blue"),axes=TRUE)
# 
# # Removing seasonal component from the signal
# seasonally_adjusted_sales_instore <- ts_sales_instore - ts_sales_components_instore$seasonal
# plot(seasonally_adjusted_sales_instore) #the signal minus the seasonal componenet
# ###############################################################################################################
# Taking only Instore Sales Quantity for further analysis
ts_sales_online<- ts(d4$OnlineSalesQuantity, frequency=52, start = c(2016,1), end = c(2018,1))
ts_sales_online <- tsclean((ts_sales_online))
plot.ts((ts_sales_online)) #Plot the complete signal

# Decomposing signal using fourier transformation
ts_sales_components_online <- decompose(ts_sales_online)

# Plotting Seasonal, Trend & Random behavior = Observed Signal
plot(ts_sales_components_online, xlab="time (2 years)",cex=5, frame.plot=TRUE,col.axis="grey",col=c("blue"),axes=TRUE)

# Removing seasonal component from the signal
seasonally_adjusted_sales_online <- ts_sales_online - ts_sales_components_online$seasonal
plot(seasonally_adjusted_sales_online) #the signal minus the seasonal componenet
# ###############################################################################################################


# install.packages("forecast")
library(forecast)

# fit <- ets(ts_sales)
# fc <- forecast(fit)
# plot(fc)
# 
# y <- msts(ts_sales, seasonal.periods=c(7,365.25))
# fit <- tbats(y)
# fc <- forecast(fit)
# plot(fc)

sum(d4$OnlineSalesQuantity)
sum(d4$InStoreSalesQuantity)


#####################################################################################################################

colnames(d4)
sample <- d4[,c(6:11,18:21,138:143)]


# Getting required data.frame = Combining Soil and weather data together
# colnames(weather3)
# colnames(soil)
# mix=merge(weather3,soil,by.x = "Location_ID",by.y = "Location_ID",all.x = FALSE)
# mix <- mix[,-10]
# mix <- mix[,-10]
# head(mix)


dim(mix)
# [1] 3683     17

#Subsetting the data
sample <- mix[,4:17]

#Scaling the values

list <- names(sample)
scaled_data <- data.frame(rownum <- 1:540350)
for(i in 1:length(list))
{
  y<-(sample[,i]-mean(sample[,i]))/(sd(sample[,i]))
  scaled_data<-cbind(scaled_data,y)
  names(scaled_data)[i+1]<-paste("scaled_",list[i])
  print(list[i])
}

colnames(scaled_data)  


#Deleting redundant column    
scaled_data <- scaled_data[,-1]

#Fix the seeds
set.seed(200)

# Combining scaled data and sample data

sample<-cbind(sample,scaled_data)
names(sample)
# [1] "w1"         "w2"         "w3"         "w4"         "w5"         "w6"         "s1"         "s2"         "s3"        
# [10] "s4"         "s5"         "s6"         "s7"         "s8"         "scaled_ w1" "scaled_ w2" "scaled_ w3" "scaled_ w4"
# [19] "scaled_ w5" "scaled_ w6" "scaled_ s1" "scaled_ s2" "scaled_ s3" "scaled_ s4" "scaled_ s5" "scaled_ s6" "scaled_ s7"
# [28] "scaled_ s8"

fit.km <- kmeans(sample[,17:30],10)
fit.km$withinss
# [1] 250931.57 876956.17 172706.42 153710.98 297693.25 501925.28  63918.28 210718.84 297289.57 406905.18
fit.km$betweenss
# [1] 4332130
colnames(sample)

# Determining optimal number of clusters
wss <- 1:15
number <- 1:15

for (i in 1:15)
{
  wss[i]<-kmeans(sample[,15:28],i)$tot.withinss
}


# ggplot2

library(ggplot2)
data<-data.frame(wss,number)
p<-ggplot(data,aes(x=number,y=wss),color="red")
p+geom_point()+scale_x_continuous(breaks=seq(1,20,1))

# Build 5 cluster model

set.seed(100)
fit.km<-kmeans(sample[,1:14],5) # We can define number of clusters here
colnames(sample)

# Merging the cluster output with original data

sample$cluster<-fit.km$cluster
max(sample$cluster)
colnames(sample)
head(sample)

# Profiling of the new clusters formed

#Cluster wise Aggregates
cmeans<-aggregate(sample[,1:14],by=list(sample$cluster),FUN=mean)
cmeans
# 
# Group.1            w1          w2           w3          w4          w5           w6       s1       s2       s3       s4       s5       s6       s7        s8
# 1       1 -0.0211719827  0.09846443  0.410721717  0.70351062  0.80310640  0.666600335 14.58819 23.32047 62.05354 16.31772 5.859843 4.794488 15.54409 6.3370079
# 2       2 -0.0055273037  0.10320057 -0.006335121  0.13071341  0.07624416  0.121678503 23.23051 52.69213 24.08450 23.78202 6.262228 5.313559 24.12506 1.7638015
# 3       3 -0.0101198748  0.11718401  0.103860863  0.29695349  0.27497068  0.316135523 25.76366 62.47398 11.71599 25.30988 6.160465 4.323110 25.47602 0.9572674
# 4       4 -0.0002614023 -0.18101420 -0.016369014  0.05276228 -0.02663594  0.007018308 29.02848 41.40970 29.61212 24.56970 6.720606 5.732879 27.70697 2.3771212
# 5       5  0.0016774373 -0.32048254  0.197722500 -0.39644145  0.02958301 -0.262328938 19.58776 33.73403 46.68413 20.11606 6.644551 5.371319 21.48356 5.6726577
# 6       6  0.0051242602 -0.06863602 -0.191841293 -0.24358087 -0.28090589 -0.238856965 21.75332 41.70186 36.54808 22.40384 6.465658 6.782189 23.93958 3.2367870
# # 
dim(cmeans)
## Visualise the clusters

#Plotting groups across two dimensions
install.packages("ggplot2")
library(ggplot2)
mix<-cbind(d4,sample)
colnames(mix)
mix <- mix[,-c(18:31)]
mix <- mix[,-c(18:31)]
colnames(mix)

write.csv(mix,"mix.csv")
# mix <- read.csv("mix.csv")

#For 5 clusters
#Year vs Weather

# layout(matrix(c(1,1,2,3),2,2, byrow = TRUE))

par(mfrow=c(2,2))

p<-ggplot(sample,aes(x=OnlineSalesQuantity,y=InStoreSalesQuantity))
p+geom_point(aes(colour=as.factor(cluster)))

p<-ggplot(sample,aes(x=OnlineTrafficToAvgVar,y=InStoreTrafficToAvgVar))
p+geom_point(aes(colour=as.factor(cluster)))

p<-ggplot(sample,aes(x=OnlineDiscountPercent,y=InStoreDiscountPercent))
p+geom_point(aes(colour=as.factor(cluster)))

head(sample)

colnames(sample)

mix <- mix[,c(5:11,209)]
write.csv(mix,"mix.csv")
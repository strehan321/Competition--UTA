
d1<-read.csv("D:/UTD/Competitions/Arlington/Data/d1.csv") # d1 is merged Sales and Traffic on Date
d2<-read.csv("D:/UTD/Competitions/Arlington/Data/d2.csv") # d2 is merged file of Sales traffic and product features 
history<-read.csv("D:/UTD/Competitions/Arlington/Data/history.csv")  # history is product_featured historical file
sales<-read.csv("D:/UTD/Competitions/Arlington/Data/sales.csv")
product1<-read.csv("D:/UTD/Competitions/Arlington/Data/product1.csv") 
feb_2018<-read.csv("D:/UTD/Competitions/Arlington/feb_2018.csv")


names(d2)
names(history)

history<-history[,-c(1:3)]

colnames(history)[3]<-"SKU"
d3<-merge(d2,history,by="SKU")

# Handeling Date 
d3$Start.Date<-as.character(d3$Start.Date)
d3$End.Date<-as.character(d3$End.Date)
d3$ActivityDate<-as.character(d3$ActivityDate)

d3$Start.Date<-as.Date(d3$Start.Date,"%Y-%m-%d")
d3$End.Date<-as.Date(d3$End.Date,"%Y-%m-%d")
d3$ActivityDate<-as.Date(d3$ActivityDate,"%Y-%m-%d")

d4<-d3[which(d3$ActivityDate>=d3$Start.Date & d3$ActivityDate<=d3$Start.Date),]
write.csv(d4,file="D:/UTD/Competitions/Arlington/Data/d4.csv")


# write.csv(d4,"D:/d4.csv")
# write.csv(instore,"D:/instore.csv")
# write.csv(online,"D:/online.csv")
# 
# d4<-read.csv("D:/d4.csv")



colnames(d4)

d41 <- d4[,17:20] 
d42 <- d4[,10:14]
install.packages("corrgram")
install.packages("corrplot")
library(corrgram)
library(corrplot)
# correlation plot between vehicles 
corrplot(cor(d42), method = "circle", type = "upper")
corrplot(cor(d41), method = "circle", type = "upper")

corrplot(cor(d42), method = "pie", type = "upper")
corrplot(cor(d41), method = "pie", type = "upper")

corrplot(cor(d42), method = "number", type = "upper")
corrplot(cor(d41), method = "number", type = "upper")

str(d41)

#################################################################################################

names(instore)
names(online)

instore<-instore[,-c(11,12)]
online<-online[,-c(11,12)]
instore<-instore[,-c(10,14,5)]
online<-online[,-c(11,12)]

instoreSKU<-instore$SKU
onlineSKU<-online$SKU

instore<-instore[,-1]
online<-online[,-1]

model_1=lm(InStoreSalesQuantity ~ ., data = instore)
model_summary = summary(model_1)
(model_summary$sigma)^2
model_summary$r.squared
model_summary$adj.r.squared
model_summary
# 
# Call:
#   lm(formula = InStoreSalesQuantity ~ ., data = instore)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -495.2   -60.8   -27.1    26.0 11376.2 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             32.49979    3.73767   8.695  < 2e-16 ***
#   InStoreDiscountPercent  -0.72263    0.12630  -5.721 1.06e-08 ***
#   InStoreTrafficToAvgVar   1.91385    0.02734  70.008  < 2e-16 ***
#   X2U                    -10.83966    4.40832  -2.459   0.0139 *  
#   HD                       5.12111  199.23393   0.026   0.9795    
# Normal                  83.17066    3.40564  24.421  < 2e-16 ***
#   OSZ                     48.57557  199.26178   0.244   0.8074    
# Is_EM                    4.16512    2.23269   1.866   0.0621 .  
# Is_OB_                  12.31417   12.97728   0.949   0.3427    
# Is_OP1                   0.73153    2.84066   0.258   0.7968    
# date_difference         -0.13980    0.03466  -4.033 5.51e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 199.2 on 41949 degrees of freedom
# Multiple R-squared:  0.1364,	Adjusted R-squared:  0.1362 
# F-statistic: 662.4 on 10 and 41949 DF,  p-value: < 2.2e-16

online<-online[,-c(5,14,10)]
model_2=lm(OnlineSalesQuantity ~ ., data = online)
model_summary2 = summary(model_2)
(model_summary2$sigma)^2
model_summary2$r.squared
model_summary2$adj.r.squared
model_summary2
# Call:
#   lm(formula = OnlineSalesQuantity ~ ., data = online)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -59.99   -9.14   -4.12    2.00 1258.82 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            3.823026   0.557345   6.859 7.01e-12 ***
#   OnlineDiscountPercent  0.092995   0.024609   3.779 0.000158 ***
#   OnlineTrafficToAvgVar  0.348069   0.006001  58.002  < 2e-16 ***
#   X2U                    1.241543   0.680236   1.825 0.067983 .  
# HD                    -5.948750  30.870473  -0.193 0.847195    
# Normal                 6.770963   0.527607  12.833  < 2e-16 ***
#   OSZ                    1.197997  30.872483   0.039 0.969046    
# Is_EM                  1.885135   0.345994   5.448 5.11e-08 ***
#   Is_OB_                -2.447029   2.010760  -1.217 0.223624    
# Is_OP1                 0.836747   0.439332   1.905 0.056840 .  
# date_difference       -0.048138   0.005366  -8.970  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 30.86 on 41949 degrees of freedom
# Multiple R-squared:  0.08749,	Adjusted R-squared:  0.08728 
# F-statistic: 402.2 on 10 and 41949 DF,  p-value: < 2.2e-16


feb<-read.csv("D:/ar_feb_data.csv")
feb$Start.Date <- as.character(feb$Start.Date)
feb$End.Date <- as.character(feb$End.Date)

feb$Start.Date <- as.Date(feb$Start.Date,"%m/%d/%Y" )
feb$End.Date <- as.Date(feb$End.Date,"%m/%d/%Y")

#str(feb$date_difference)

feb$date_difference <- as.numeric(feb$End.Date -feb$Start.Date)


feb1 <- feb
feb2 <- feb
feb3 <- feb
feb4 <- feb
feb5 <- feb

# As feb data doesnt have the Discount percentage and traffic to avg variance we
# tried to create those variables based on distribution in previous product features data

hist(instore$InStoreTrafficToAvgVar)
colnames(feb)

feb1$InStoreDiscountPercent <- 1
feb1$InStoreTrafficToAvgVar <- -10

feb2$InStoreDiscountPercent <- 2
feb2$InStoreTrafficToAvgVar <- -20

feb3$InStoreDiscountPercent <- 3
feb3$InStoreTrafficToAvgVar <- 0

feb4$InStoreDiscountPercent <- 4
feb4$InStoreTrafficToAvgVar <- 10

feb5$InStoreDiscountPercent <- 5
feb5$InStoreTrafficToAvgVar <- 20

# [1] "SKU"               "Dept"              "Class"             "Description"       "Code"             
# [6] "Vehicle"           "Start.Date"        "End.Date"          "Is_EM"             "Is_OB_"           
# [11] "Is_OP1"            "Is_OSM"            "X2U"               "HD"                "Normal"           
# [16] "OSZ"               "XR"                "IsSave"            "IsOnlineExclusive" "IsPier1ToYou"     
# [21] "IsInStoreOnly"     "IsPier1ToGo"       "IsInStock"         "IsNewFlag"         "IsClearance"      
# [26] "IsSearchable" 


colnames(instore)
# 
# [1] "SKU"                    "InStoreSalesQuantity"   "InStoreDiscountPercent" "InStoreTrafficToAvgVar"
# [5] "X2U"                    "HD"                     "Normal"                 "OSZ"                   
# [9] "Is_EM"                  "Is_OB_"                 "Is_OP1"                 "date_difference" 


colnames(online)
# 
# [1] "SKU"                   "OnlineSalesQuantity"   "OnlineDiscountPercent" "OnlineTrafficToAvgVar"
# [5] "X2U"                   "HD"                    "Normal"                "OSZ"                  
# [9] "Is_EM"                 "Is_OB_"                "Is_OP1"                "date_difference" 

#prediction for different combination of discount percentage and traffic to avg variance
instore_p1<-predict(model_1,feb1)
instore_p2<-predict(model_1,feb2)
instore_p3<-predict(model_1,feb3)
instore_p4<-predict(model_1,feb4)
instore_p5<-predict(model_1,feb5)
instore_prediction<-cbind(as.data.frame(instore_p1),as.data.frame(instore_p2),as.data.frame(instore_p3),as.data.frame(instore_p4),as.data.frame(instore_p5))


mean(predict(model_1,feb1))
mean(predict(model_1,feb2))
mean(predict(model_1,feb3))
mean(predict(model_1,feb4))
mean(predict(model_1,feb5))

feb1$OnlineDiscountPercent <- 1
feb1$OnlineTrafficToAvgVar <- -10

feb2$OnlineDiscountPercent <- 2
feb2$OnlineTrafficToAvgVar <- -20

feb3$OnlineDiscountPercent <- 3
feb3$OnlineTrafficToAvgVar <- -0

feb4$OnlineDiscountPercent <- 4
feb4$OnlineTrafficToAvgVar <- 10

feb5$OnlineDiscountPercent <- 5
feb5$OnlineTrafficToAvgVar <- 20

hist(online$OnlineDiscountPercent)
hist(online$OnlineTrafficToAvgVar)


#prediction for different combination of discount percentage and traffic to avg variance
#for online store
summary(d4$OnlineSalesQuantity)
online_p1<-predict(model_2,feb1)
online_p2<-predict(model_2,feb2)
online_p3<-predict(model_2,feb3)
online_p4<-predict(model_2,feb4)
online_p5<-predict(model_2,feb5)

colnames(prediction_data_feb)

mean(predict(model_2,feb1))
mean(predict(model_2,feb2))
mean(predict(model_2,feb3))
mean(predict(model_2,feb4))
mean(predict(model_2,feb5))

plot(prediction_data_feb$Start.Date,prediction_data_feb$online_p1, col = "red")
par(new = TRUE)
lines(prediction_data_feb$Start.Date,prediction_data_feb$online_p1,col = "green")

online_prediction<-cbind(as.data.frame(online_p1),as.data.frame(online_p2),as.data.frame(online_p3),as.data.frame(online_p4),as.data.frame(online_p5))
prediction_data_feb<-cbind(online_prediction,instore_prediction,feb5)
write.csv(prediction_data_feb,"D:/UTD/Competitions/Arlington/Data/feb_prediction.csv")


#################################################################################################################################################################################

# calculating the relative importance of different marketing variables and structural components

#install.packages("relaimpo")# recquired for calculating relative importance of variables
library(relaimpo)
relative_importance=calc.relimp(model_1, type = c("car"), rela = TRUE) #used 'car' as cost function; rela=TRUE give % that a variable contributed to the adjusted R2
relative_importance
# Response variable: InStoreSalesQuantity 
# Total response variance: 45933.6 
# Analysis based on 41960 observations 
# 
# 10 Regressors: 
#   InStoreDiscountPercent InStoreTrafficToAvgVar X2U HD Normal OSZ Is_EM Is_OB_ Is_OP1 date_difference 
# Proportion of variance explained by model: 13.64%
# Metrics are normalized to sum to 100% (rela=TRUE). 
# 
# Relative importance metrics: 
#   
#   car
# InStoreDiscountPercent 5.824011e-03
# InStoreTrafficToAvgVar 7.669618e-01
# X2U                    3.861656e-02
# HD                     5.276400e-06
# Normal                 1.775096e-01
# OSZ                    1.132442e-06
# Is_EM                  2.178196e-03
# Is_OB_                 1.922350e-05
# Is_OP1                 2.468452e-03
# date_difference        6.415759e-03

relative_imp=c(5.824011e-03,7.669618e-01,3.861656e-02,5.276400e-06,1.775096e-01,1.132442e-06,2.178196e-03,1.922350e-05,2.468452e-03,6.415759e-03)
online_sales_quantity_sum=sum(online$OnlineSalesQuantity)
relative_contribution_of_each_variable=relative_imp*online_sales_quantity_sum
relative_contribution_of_each_marketing=relative_contribution_of_each_variable[1:10]
# 1] 2.556729e+03 3.366947e+05 1.695259e+04 2.316329e+00 7.792636e+04 4.971398e-01 9.562237e+02 8.439078e+00
# [9] 1.083645e+03 2.816505e+03

#################################################################################################################################################################################
#Relative importance between channels
#############################################################################################

# Trend Line for the Instore Prediction
onlineSales<- ts(instore_prediction$instore_p1, frequency=50,start=c(2018,1)) #Assuming the data is for the last 2 years; Doesnt change the outcome of the analysis 
plot.ts((onlineSales)) #Plot the complete signal
onlineSales_components <- decompose(onlineSales) #Sort of a fourier transformation to decompose signal
plot(onlineSales_components, xlab="time - Feb Month",cex=5, frame.plot=TRUE,col.axis="grey",col=c("blue"),axes=FALSE)
seasonally_adjusted_sales <- onlineSales - onlineSales_components$seasonal
plot(seasonally_adjusted_sales) #the signal minus the seasonal com


# Trend Line for the Online Prediction
onlineSales<- ts(online_prediction$online_p1, frequency=50,start=c(2018,1)) #Assuming the data is for the last 2 years; Doesnt change the outcome of the analysis 
plot.ts((onlineSales)) #Plot the complete signal
onlineSales_components <- decompose(onlineSales) #Sort of a fourier transformation to decompose signal
plot(onlineSales_components, xlab="time - Feb Month",cex=5, frame.plot=TRUE,col.axis="grey",col=c("blue"),axes=FALSE)
seasonally_adjusted_sales <- onlineSales - onlineSales_components$seasonal
plot(seasonally_adjusted_sales) #the signal minus the seasonal com





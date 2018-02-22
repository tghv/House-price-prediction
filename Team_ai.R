#clear environment
rm(list=ls())

#set path
setwd("G:/AV/data hack case study/Data Hack Round 3 Case Study")
load("AV.RData")
#load libraries
library(dplyr)
library(MLmetrics)
library(lubridate)
library(dummies)
library(missForest)
library(xgboost)
library(data.table)
library(randomForest)
library(DMwR)
library(nnet)
library(caret)
library(keras)
library(tidyr)
library(ROSE)
library(smotefamily)
library(rpart.plot)
library(rpart)
library(RColorBrewer)
library(ROCR)
library(C50)
library(CHAID)
library(woe)
library(Information)
library(InformationValue)
library(gridExtra)
library(devtools)
library(e1071)
library(MASS)
library(car)
library(psych)
library(corrplot)
library(zoo)
library(h2o)
library(sqldf)
library(ggplot2)

#read data

full1<-read.csv("Copy of Housing_Resale_Data.csv",stringsAsFactors = F)

full<-full1

# EDA and preprocessing
str(full)
table(full$town)

colSums(is.na(full))
names(full)

summary(full)
summary(full$resale_price)

#distribution of target variable(resale_price)  
ggplot(full,aes(x=resale_price,fill=..count..))+geom_histogram()
ggplot(full,aes(x=sqrt(resale_price),fill=..count..))+geom_histogram()


#resale_price vs town
full$town<-as.factor(full$town)

ggplot(full,aes(resale_price))+geom_histogram(aes(fill=town),position = position_stack(reverse = T))

#flat type
table(full$flat_type)
full$flat_type<-as.factor(full$flat_type)

ggplot(full,aes(x=resale_price))+geom_histogram(aes(fill=flat_type))

#flat model
table(full$flat_model)
ggplot(full,aes(x=resale_price))+geom_histogram(aes(fill=flat_model))


#month
year<-substring(full$month,1,4)
year<-as.numeric(year_data)

full<-cbind(full,year)

#new feature age
full$age<-full$year-full$lease_commence_date

#removing month.year and lease_commence_date column
full$month<-NULL
full$year<-NULL
full$lease_commence_date<-NULL

#block vs price
a1<-ggplot(full,aes(x=block,y=resale_price))+geom_point()+geom_smooth(method = lm)

#floor sqm vs price
a2<-ggplot(full,aes(x=floor_area_sqm,y=resale_price))+geom_point()+geom_smooth(method = lm)

#age vs price
a3<-ggplot(full,aes(x=age,y=resale_price))+geom_point()+geom_smooth(method = lm)

#storey range
ggplot(full,aes(x=resale_price,fill=storey_range))+geom_histogram()

#grid.arrange(a1,a2,a3)

#street name

full$street_name<-as.factor(full$street_name)

#storey_range
full$storey_range<-as.factor(full$storey_range)

#flat_model
full$flat_model<-as.factor(full$flat_model)

#correlation between numeric attributes and target variable
cor(full[,c(6,8,9)])

full$block<-as.factor(full$block)
full$block<-as.numeric(full$block)
full$block<-NULL

#street name
full$street_name<-NULL

boxplot(full[,c(4,6,7)])


#

housing_data<-full1



#price according to area
price_town<-sqldf("select avg(resale_price) as average_price,town from housing_data group by town order by avg(resale_price) desc")
price_town
price_town$town<-as.factor(price_town$town)

#install.packages("scales")
library(scales)
point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

ggplot(price_town,aes(reorder(x=town,average_price),y=(average_price)))+geom_bar(stat = "identity")+coord_flip()

#Bukit timah
#With proximity to many prestigious education academies like Hwa Chong Institution, Hwa Chong International School, Nanyang Girls' 
#High School and National Junior College, Bukit Timah is an extremely popular neighborhood
#Located 4 kms from Bukit timah hill,Pleasant climate
# Bukit Timah lies roughly 10 kilometres (6.2 mi) from the Central Business District

#Punggol
#Punggol is selected to be developed as Singapore's first Eco-Town
#The 4.2km-long Punggol Waterway is now one of the most recognisable landmarks 
#in the town. Residents and visitors can take part in a gamut of 
#leisure activities along the waterway and enjoy the scenic views as 
#they walk along the landscaped promenade

#Bishan 
#less dwelling units and less populated area in singapore-19,664


#Ang mo kio Singapore is largely undeveloped, being mainly used for agricultural purposes, with uninhabited 
#plots of land usually covered in dense secondary forest or swamps


#Yishun is the most terrifying place to live in Singapore
#Yishun-on and off rat issues,drug trafficking and huge number of murders


colnames(housing_data)

#Price according to streets
price_street<-sqldf("select avg(resale_price) as average_price,street_name,town from housing_data group by street_name order by average_price desc")
price_street

h_street<-as.data.frame(head(price_street,10))

ggplot(h_street,aes(reorder(x=street_name,average_price),y=(average_price)))+geom_bar(stat = "identity")+coord_flip()


#Cantonment road of central area town is the costliest, 
#it is the city centre of Singapore

#Central Area
#he Central Area is one of the most densely developed places in Singapore, with a large mix of commercial and residential developments 
#packed into a space of only 1784 hectares
#Parliament and rest govt offices in central area


#Price according to flat_type
price_flattype<-sqldf("select count(flat_type) as frequency,flat_type from housing_data group by flat_type order by count(flat_type) desc")
price_flattype

ggplot(price_flattype,aes(reorder(x=flat_type,frequency),y=(frequency)))+geom_bar(stat = "identity")+coord_flip()


# frequency            flat_type
# 1    185387           4 ROOM
# 2    144749           3 ROOM
# 3    106811           5 ROOM
# 4     35590        EXECUTIVE
# 5      4688           2 ROOM
# 6       419           1 ROOM
# 7       189     MULTI-GENERATION


#Multi generation flats
# The couple must be eligible to buy a flat under the Public Scheme.
#At least 1 parent must be a Singapore Citizen or Singapore Permanent Resident



colnames(housing_data)

#Most resale price in singapore
max_sale<-sqldf("select street_name,resale_price from housing_data  order by resale_price desc")
head(max_sale,10)


#Resale price according to block
price_block<-sqldf("select avg(resale_price),block from housing_data group by block order by avg(resale_price) desc")
head(price_block)

#Resale price according to storey_range
price_storeyrange<-sqldf("select avg(resale_price),storey_range from housing_data group by storey_range order by avg(resale_price) desc")
price_storeyrange

#Price according to flat_model
price_flatmodel<-sqldf("select avg(resale_price),flat_model from housing_data group by flat_model order by avg(resale_price) desc")
price_flatmodel



#spltting of data
set.seed(100)

tr<-sample(1:nrow(full),0.8*nrow(full))
train<-full[tr,]
val<-full[-tr,]

#Model building

#linear regression

lm_model<-lm((resale_price)~.,data = train)
summary(lm_model)

step.model<-stepAIC(lm_model)

pr_val<-predict(step.model,val)

RMSE((pr_val),(val$resale_price)) # 84865
regr.eval(pr_val,val$resale_price)#RMSE 84865
                                  #MAE 64851
#decision tree
rp<-rpart((resale_price)~.,data = train)

pred_rp<-predict(rp,val)
RMSE((pred_rp),(val$resale_price))# 97019
regr.eval((pred_rp),(val$resale_price)) # RMSE 97019
                                        # MAE 75443.5

#h2o
h2o.init(nthreads = -1)

train.hex<-as.h2o(train)
val.hex<-as.h2o(val)

#random forest
y<-c("resale_price")
x<-setdiff(names(train.hex),y)

rf<-h2o.randomForest(x,y,training_frame = train.hex,validation_frame = val.hex,ntrees = 10,seed = 123)
summary(rf)## RMSE:  47261.08
           ## MAE:  32055.87 on val


h2o.varimp(rf)
h2o.varimp_plot(rf)


var<-cbind(h2o.varimp(rf)$scaled_importance,h2o.varimp(rf)$variable)
var<-as.data.frame(var)  
colnames(var)<-c("scaled_importance","variable")

#GBM
gbm_model<-h2o.gbm(x,y,training_frame = train.hex,validation_frame = val.hex,seed = 200)
summary(gbm_model)## RMSE:  64480.11
                  ## MAE:  47825.96  on val using default parameters

#xgboost without h2o
xtrain<-train[,-c(6)]
ytrain<-(train$resale_price)

xval<-val[,-6]
yval<-(val$resale_price)

xgb_model<-xgboost(data.matrix(xtrain),data.matrix(ytrain),cv=5,objective="reg:linear",nrounds=500,max.depth=10,eta=0.05,colsample_bytree=0.5,seed=235,metric="rmse",importance=T)

pred_xgb<-predict(xgb_model,data.matrix(xval))
RMSE((pred_xgb),(val$resale_price))
regr.eval((pred_xgb),(val$resale_price))## RMSE: 48091.65 without sqrt, with RMSE: 48116
                                       ## MAE:  33113.13 without sqrt,with MAE: 32763.66 using cv=5,  nrounds=500,ets=0.05


save.image(file="AV.RData")
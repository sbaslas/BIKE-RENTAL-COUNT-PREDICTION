### clear environment ###
rm(list=ls())

### load the libraries  ###

library(ggplot2)
library(corrgram)
library(corrplot)


###set working directory###

setwd("D:/DATA SCIENCE STUDY METERIAL/Projects/Bike renting")
getwd()


### Load Bike renting Data CSV file ###

df=read.csv("day.csv",header = T)

head(df)
###########################################################################
####################   1.3 Expletory Data Analysis   ######################
###########################################################################


#####################  1.3.1. Data Understanding  #########################

dim(df)      # checking the dimension of data frame.

str(df)      # checking datatypes of all columns.

####################  1.3.2. Data Pre Observation  ########################

#store categorical and continuous Variable column names

cat_var=c("season","yr","mnth","holiday","weekday","workingday","weathersit")

numeric_Var=c('temp', 'atemp', 'hum', 'windspeed', 'casual', 'registered', 'cnt')

#convert the data type of categorical variables to factor from integer

for(i in c(3:9)){
  
  df[,i]=as.factor(df[,i])
}

str(df)                                 #checking datatypes of all columns

summary(df[,numeric_Var])               # checking numerical variables
summary(df[,cat_var],maxsum=13)         # checking categorical variables

# bar graph for categorical variables

plot_bar <- function(cat, y, fun){
  gp = aggregate(x = df[, y], by=list(cat=df[, cat]), FUN=fun)
  ggplot(gp, aes_string(x = 'cat', y = 'x'))+
    geom_bar(stat = 'identity',fill = "aquamarine3")+
    labs(y = y, x = cat)+theme(panel.background = element_rect("antiquewhite"))+
    theme(plot.title = element_text(size = 9))+
    ggtitle(paste("Bar plot for",y,"wrt to",cat))
}

PB1=plot_bar('season', 'cnt', 'sum')
PB2=plot_bar('yr', 'cnt', 'sum')
PB3=plot_bar('mnth', 'cnt', 'sum')
PB4=plot_bar('weekday','cnt','sum')
PB5=plot_bar('weathersit', 'cnt', 'sum')
gridExtra::grid.arrange(PB1,PB2,PB3,PB4,PB5,ncol=3)

# Chacking VIF for skewness

cnames=c('temp', 'atemp', 'hum', 'windspeed')
library(propagate)
for(i in cnames){
  print(i)
  skew= skewness(df[,i])
  print(skew)
}                                          #windspeed variable is right skewed in dataset.

# histogram for continuous variables

hist_plot <- function(column, dataset){
  hist(x=dataset[,column],col="Green",xlab=column,ylab="density",
       main=paste("Histogram of ", column))
}

PH1=hist_plot('temp',dataset=df)
PH2=hist_plot('atemp',dataset=df)
PH3=hist_plot('hum',dataset=df)
PH4=hist_plot('windspeed',dataset=df)


###########################################################################
####################   2.1.	Data Preprocessing   ##########################
###########################################################################


###################  2.1.1.	Missing Value Analysis  #######################

miss_val=data.frame(lapply(df,function(x) sum(is.na(x))))
miss_val


###################  2.1.2. outlier analysis #############################

numeric_index=sapply(df,is.numeric)
numeric_data=df[,numeric_index]
cnames=colnames(numeric_data)       #selecting numerical variables

#create Box plot for outlier analysis

for(i in 1:length(cnames)){
  assign(paste0("AB",i),ggplot(aes_string(x="cnt",y=(cnames[i])),data=subset(df))+
           geom_boxplot(outlier.color = "Red",outlier.shape = 18,outlier.size = 2,
                        fill="Purple")+theme_get()+
           stat_boxplot(geom = "errorbar",width=0.5)+
           labs(x="Count of Bike",y=cnames[i])+
           ggtitle("Boxplot of count of bikes with",cnames[i]))
}

gridExtra::grid.arrange(AB2,AB3,AB4,AB5,ncol=4)   # plot all graph 

#Replace outliers with NA

for(i in cnames){
  print(i)
  outlier= df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(length(outlier))
  df[,i][df[,i] %in% outlier]=NA
}

sum(is.na(df))

#Impute outliers by median method

df$hum[is.na(df$hum)]=median(df$hum,na.rm=TRUE)
df$windspeed[is.na(df$windspeed)]=median(df$windspeed,na.rm=TRUE)
df$casual[is.na(df$casual)]=median(df$casual,na.rm=TRUE)

summary(df[,numeric_Var])                        #summary of dataset for numeric variables


#########################  2.1.3.Feature Selection  ###############################


####  for continuous variables ####

# correlation plot for numerical feature

corrgram(df[,numeric_Var], order = FALSE,
         upper.panel = panel.cor, text.panel = panel.txt,
         main = "Correlation Plot for bike data set")

# heatmap plot for numerical features

corrplot(cor(df[,numeric_Var]), method = 'color', type = 'lower',title = "Heatmap plot for bike data set")


####  for categorical Variable  ####

#Anova analysis for categorical variable with target numeric variable-
for(i in cat_var){
  print(i)
  Anova_result= summary(aov(formula = cnt~df[,i],df))
  print(Anova_result)
}


######################  2.1.5.	Data after EDA and preprocessing  ################

df= subset(df,select=-c(instant,dteday,atemp,casual,registered,holiday,weekday,workingday))


head(df)

write.csv(df,"bike data for model development.csv", row.names=FALSE )

# change categorical to numeric making bin for regression model

cat_index=sapply(df,is.factor)
cat_data=df[,cat_index]
cat_var=colnames(cat_data)

library(dummies)
df= dummy.data.frame(df,cat_var)

###########################################################################
####################   2.2. Model Development   ###########################
###########################################################################
  

#########################  2.2.1 Model building  #########################

#clear all the data except final data set.

data=df
df=data

library(DataCombine)
rmExcept("data")


#Function for Error metrics to calculate the performance of model-
mape= function(y,y1){
  mean(abs((y-y1)/y))*100
}

#Function for r2 to calculate the goodness of fit of model-
rsquare=function(y,y1){
  cor(y,y1)^2
}


# devide the data in train and test

set.seed(123)
train_index= sample(1:nrow(data),0.8*nrow(data))
train= data[train_index,]
test= data[-train_index,]

################## 2.2.1.desision tree for regression  ##########################

library(rpart)

fit=rpart(cnt~.,data=train,method = "anova") #model development on train data

DT_test=predict(fit,test[,-25])            #predict test data
DT_train= predict(fit,train[,-25])         #predict train data

DT_MAPE_Test = mape(test[,25],DT_test)     # MAPE calculation for test data
DT_MAPE_Train = mape(train[,25],DT_train)  # MAPE calculation for train data

DT_r2_test=rsquare(test[,25],DT_test)      # r2 calculation for test data
DT_r2_train= rsquare(train[,25],DT_train)  # r2 calculation for train data


### 2.2.2. Random forest for regression ###

library(randomForest)

RF_model= randomForest(cnt~.,train,ntree=100,method="anova") #Model development on train data

RF_test= predict(RF_model,test[-25])      #Prediction on test data
RF_train= predict(RF_model,train[-25])    #Prediction on train data

RF_MAPE_Test=mape(test[,25],RF_test)      #MAPE calculation of test data-
RF_MAPE_Train=mape(train[,25],RF_train)   #MAPE calculation of train data

RF_r2_test=rsquare(test[,25],RF_test)     #r2 calculation for test data-
RF_r2_train=rsquare(train[,25],RF_train)  #r2 calculation for train data-




###  2.2.3. Linear Regression ###


LR_model= lm(cnt~.,train)               #Model devlopment on train data
summary(LR_model)

LR_test= predict(LR_model,test[-25])      #prediction on test data
LR_train= predict(LR_model,train[-25])    #prediction on train data

LR_MAPE_Test=mape(test[,25],LR_test)      #MAPE calculation of test data
LR_MAPE_Train=mape(train[,25],LR_train)   #MAPE calculation of train data

LR_r2_test=rsquare(test[,25],LR_test)     #r2 calculation for test data
LR_r2_train=rsquare(train[,25],LR_train)  #r2 calculation for train data



### 2.2.4. Gradient Boosting ###

library(gbm)

GB_model = gbm(cnt~., data = train, n.trees = 100, interaction.depth = 2) #Model devlopment on train data

GB_test = predict(GB_model, test[-25], n.trees = 100)      #prediction on test data
GB_train = predict(GB_model, train[-25], n.trees = 100)    #prediction on train data

GB_MAPE_Test=mape(test[,25],GB_test)                       
GB_MAPE_Train=mape(train[,25],GB_train)                    #Mape calculation of train data

GB_r2_test=rsquare(test[,25],GB_test)                      #r2 calculation for test data-
GB_r2_train=rsquare(train[,25],GB_train)                   #r2 calculation for train data-




Result= data.frame('Model'=c('Decision Tree for Regression','Random Forest',
                           'Linear Regression','Gradient Boosting'),
                   'MAPE_Train'=c(DT_MAPE_Train,RF_MAPE_Train,LR_MAPE_Train,GB_MAPE_Train),
                   'MAPE_Test'=c(DT_MAPE_Test,RF_MAPE_Test,LR_MAPE_Test,GB_MAPE_Test),
                   'R-Squared_Train'=c(DT_r2_train,RF_r2_train,LR_r2_train,GB_r2_train),
                   'R-Squared_Test'=c(DT_r2_test,RF_r2_test,LR_r2_test,GB_r2_test))

Result           #Random forest and Gradient Bosting have best fit model for the data.


#########################  2.2.2.	Hyperparameter Tuning  #########################

#Random Search CV in Random Forest

library(caret)

control = trainControl(method="repeatedcv", number=10, repeats=3,search='random')


RRF_model = caret::train(cnt~., data=train, method="rf",trControl=control,tuneLength=10)   #model devlopment on train data
best_parameter = RRF_model$bestTune                            #Best fit parameters
print(best_parameter)
#mtry=7 As per the result of best_parameter

RRF_model = randomForest(cnt ~ .,train, method = "rf", mtry=7,importance=TRUE)        #build model based on best fit

RRF_test= predict(RRF_model,test[-25])                        #Prediction on test data
RRF_train= predict(RRF_model,train[-25])                      #Prediction on train data


RRF_MAPE_Test = mape(test[,25],RRF_test)                      #Mape calculation of test data
RRF_MAPE_Train = mape(train[,25],RRF_train)                   #Mape calculation of train data


RRF_r2_test=rsquare(test[,25],RRF_test)                      #r2 calculation for test data
RRF_r2_train= rsquare(train[,25],RRF_train)                  #r2 calculation for train data


# Grid Search CV in Random Forest

control = trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid = expand.grid(.mtry=c(6:18))


GRF_model= caret::train(cnt~.,train, method="rf", tuneGrid=tunegrid, trControl=control)     #model devlopment on train data
best_parameter = GRF_model$bestTune                           #Best fit parameters
print(best_parameter)
#mtry=9 As per the result of best_parameter

GRF_model = randomForest(cnt ~ .,train, method = "anova", mtry=9)                    #build model based on best fit

GRF_test= predict(GRF_model,test[-25])                      #Prediction on test data
GRF_train= predict(GRF_model,train[-25])                    #Prediction on train data


GRF_MAPE_Test = mape(test[,25],GRF_test)                    #Mape calculation of test data
GRF_MAPE_Train = mape(train[,25],GRF_train)                 #Mape calculation of train data


GRF_r2_test=rsquare(test[,25],GRF_test)                     #r2 calculation for test data
GRF_r2_train= rsquare(train[,25],GRF_train)                 #r2 calculation for train data



## Random Search CV in Gradient Boosting

control = trainControl(method="repeatedcv", number=5, repeats=1)

RGB_model = caret::train(cnt~., data=train, method="gbm",trControl=control,tuneLength=10)     #model devlopment on train data
best_parameter = RGB_model$bestTune                        #Best fit parameters
print(best_parameter)
# n.trees=100,interaction.depth=5,shrinkage=0.1,n.minobsinnode=10

RGB_model = randomForest(cnt ~ .,train, method = "anova", n.trees=100,
                         interaction.depth=5,shrinkage=0.1,n.minobsinnode=10)             #build model based on best fit


RGB_test= predict(RGB_model,test[-25])                     #Prediction on test data
RGB_train= predict(RGB_model,train[-25])                   #Prediction on train data


RGB_MAPE_Test = mape(test[,25],RGB_test)                   #Mape calculation of test data
RGB_MAPE_Train = mape(train[,25],RGB_train)                #Mape calculation of train data


RGB_r2_test=rsquare(test[,25],RGB_test)                    #r2 calculation for test data
RGB_r2_train= rsquare(train[,25],RGB_train)                #r2 calculation for train data



### Grid Search CV in Gradient Boosting

control = trainControl(method="repeatedcv", number=5, repeats=2, search="grid")
tunegrid = expand.grid(n.trees = seq(2565,2575, by = 2),
                       interaction.depth = c(2:4), 
                       shrinkage = c(0.01,0.02),
                       n.minobsinnode = seq(18,22, by = 2))

-
GGB_model= caret::train(cnt~.,train, method="gbm", tuneGrid=tunegrid, trControl=control)      #model devlopment on train data
best_parameter = GGB_model$bestTune                       #Best fit parameters
print(best_parameter)
# n.trees = 2567,interaction.depth = 3,shrinkage = 0.01,n.minobsinnode = 18 as per best fit output

GGB_model = randomForest(cnt ~ .,train, method = "anova", n.trees = 2567,
                         interaction.depth = 3,shrinkage = 0.01,n.minobsinnode = 18)          #build model based on best fit

GGB_test= predict(GGB_model,test[-25])                    #Prediction on test data
GGB_train= predict(GGB_model,train[-25])                  #Prediction on train data


GGB_MAPE_Test = mape(test[,25],GGB_test)                  #Mape calculation of test data
GGB_MAPE_Train = mape(train[,25],GGB_train)               #Mape calculation of train data


GGB_r2_test=rsquare(test[,25],GGB_test)                   #r2 calculation for test data
GGB_r2_train= rsquare(train[,25],GGB_train)               #r2 calculation for train data


final_result= data.frame('Model'=c('Decision Tree for Regression','Random Forest',
                               'Linear Regression','Gradient Boosting',
                               'Random Search CV in Random Forest','Grid Search CV in Random Forest',
                               'Random Search CV in Gradient Boosting','Grid Search CV in Gradient Boosting'),
                     'MAPE_Train'=c(DT_MAPE_Train,RF_MAPE_Train,LR_MAPE_Train,GB_MAPE_Train,
                                    RRF_MAPE_Train,GRF_MAPE_Train,RGB_MAPE_Train,GGB_MAPE_Train),
                     'MAPE_Test'=c(DT_MAPE_Test,RF_MAPE_Test,LR_MAPE_Test,GB_MAPE_Test,
                                   RRF_MAPE_Test,GRF_MAPE_Test,RGB_MAPE_Test,GGB_MAPE_Test),
                     'R-Squared_Train'=c(DT_r2_train,RF_r2_train,LR_r2_train,GB_r2_train,
                                         RRF_r2_train,GRF_r2_train,RGB_r2_train,GGB_r2_train),
                     'R-Squared_Test'=c(DT_r2_test,RF_r2_test,LR_r2_test,GB_r2_test,
                                        RRF_r2_test,GRF_r2_test,RGB_r2_test,GGB_r2_test))

print(final_result)

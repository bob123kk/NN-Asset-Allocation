rm(list=ls(all=TRUE))
########################################################
# 0 - Load librairies and functions
########################################################

library("tibble")
library("xts")
library("dplyr")
library("tidyquant")
library("reshape2")
library("PerformanceAnalytics")
library("caret")
library("deepnet")
library("quantmod")
library("timeSeries")
library("fPortfolio")
library("rnn")
library('fGarch')

T=15000
#process parameters
eta = 0.1 #eta = 0 is equivalent to Geometric Brownian Motion
mu = 100   #the mean of the process

#GARCH volatility model
specs = garchSpec(model = list(omega = 0.01, alpha = 0.2, beta = 0.3)) 
sigma = garchSim(spec = specs, n = T)

#########################open price
P_0 = mu #starting price, known
open = rep(P_0,T)

for(i in 2:T){
  open[i] = open[i-1] + eta * (mu - open[i-1]) + sigma[i] * open[i-1]
}
tail(open)
#prices and returns
#par(mfrow=c(2,1))       

plot(open,type="l",xlab="Time", main="open", ylab = "Price")


#########################high price
mu_high<-10
P_0 = mu_high #starting price, known
high = rep(P_0,T)

for(i in 2:T){
  high[i] = high[i-1] + eta * (mu_high - high[i-1]) + sigma[i] * high[i-1]
}
high
high_value = rep(P_0,T)
for(i in 1:T){
  high_value[i] = open[i] + high[i]
}
high_value
tail(high_value)

#prices and returns
#par(mfrow=c(2,1)) 


plot(high_value,type="l",xlab="Time", main="high", ylab = "Price")
#########################low price
mu_low<-10
P_0 = mu_low #starting price, known
low = rep(P_0,T)

for(i in 2:T){
  low[i] = low[i-1] + eta * (mu_low - low[i-1]) + sigma[i] * low[i-1]
}
low
low_value = rep(P_0,T)
for(i in 1:T){
low_value[i] = open[i] - low[i]
}
low_value
tail(low_value)


plot(low_value,type="l",xlab="Time", main="low", ylab = "Price")

#########################close price

close<-rep(0,T)
for(i in 1:T){
  close[i] = open[i+1]
}

close
tail(close)

plot(close,type="l",xlab="Time", main="close", ylab = "Price")
########################################################
# 0 - form the dataset

########################################################
min<-min(length(open),length(high_value),length(low_value),length(close))
open<-c(open[1:min])
high_value<-c(high_value[1:min])
low_value<-c(low_value[1:min])
close<-c(close[1:min])

class(close)
data<-data.frame(open,high_value,low_value,close)

data<-na.omit(data)

which(data<0)

close_df<- diff(data[,4])

count = function(data){
  counts = 0
  for (x in data){
    if (x > 0){
      counts = counts +1
      
    }
  }
  return(counts/length(data))
}


check_grach <- count(close_df)
print(check_grach)
min(data)
max(data)



########################################################
# 1 - Get data ## we select data from 1999-01-04 to 2019-07-10, 
#setting trainig data set from 1999-01-04  to 2009-03-30 (which is the 535th data point),
#setting testing data set from "2017-08-14"(which is the 972th data point) to 2019-07-10,


#date_1 <- which(daily_level_data$date == '1999-01-04')
#date_2 <- which(daily_level_data$date == '2009-03-30')
#date_3 <- which(daily_level_data$date == '2017-08-14')
#date_4 <- which(daily_level_data$date == '2019-07-10')
#date_all<-c(date_1,date_2,date_3,date_4)
#date_all
########################################################

startDate <-  as.Date("1999-01-04") 
endDate <- as.Date("2019-07-10")
tickers <- c("^FTSE")
FTSE_stock_prices <- tq_get("^FTSE")
nrow(FTSE_stock_prices )
weekly_level_data<- tq_get("^FTSE", get = "stock.prices", from = startDate, to = endDate, complete_cases = FALSE,periodicity='weekly')
weekly_level_data<-na.omit(weekly_level_data)
daily_level_data<- tq_get("^FTSE", get = "stock.prices", from = startDate, to = endDate, complete_cases = FALSE)
daily_level_data<-na.omit(daily_level_data)

date_weekly <- weekly_level_data[,1] 
date_daily <- daily_level_data[,1]
test_data_weekly <- weekly_level_data[,1:5] # get Open, High, Low, Close Prices
class(test_data_weekly)
c(nrow(date_weekly),nrow(date_daily),nrow(test_data_weekly))
#######################################################################################################################################
#1.1 add new data
########################################################################################################################################################################
test_date_weekly<-data[1:nrow(date_weekly),]

test_data_weekly<-cbind(date_weekly,test_date_weekly)
class(test_data_weekly)
colnames(test_data_weekly)<-c('date','open','high','low','close')


########################################################################################################################################################################
# 2 - Split Data into j PARTS, training/validation/training
########################################################
length<-nrow(test_data_weekly)
train_date_end<-floor((length)/2)
train_date_end
training_Date<-test_data_weekly[train_date_end,1]
training_Date

validation1_date_end <- which(test_data_weekly$date == '2017-08-07')
date<-c(train_date_end,validation1_date_end)
date

features_train_data <- test_data_weekly[1:train_date_end,] 
colnames(features_train_data) <- c('Date','Open','High','Low','Close')

features_valid_data_real<- test_data_weekly[(train_date_end-32):validation1_date_end,] # need to burn 33 more data points until the first feature indicator
valid_data <- test_data_weekly[(train_date_end+1):validation1_date_end,]              #actual valid period                                          
colnames(features_valid_data_real) <- c('Date','Open','High','Low','Close')

features_valid_data_test_real<-test_data_weekly[(validation1_date_end-32):length,] #need to burn 33 more data points until the first feature indicator
valid_data_test<- test_data_weekly[(validation1_date_end+1):length,]        # actual test period
colnames(features_valid_data_test_real) <- c('Date','Open','High','Low','Close')
Date<-test_data_weekly[validation1_date_end+1,1]
Date<-data.frame(Date)
test_startDate<-as.Date(Date[1,1])
test_endDate <- as.Date("2019-07-10")
daily_date_features_valid_data_test<- tq_get("^FTSE", get = "stock.prices", from = test_startDate, to = test_endDate, complete_cases = FALSE)
daily_date_features_valid_data_test<-na.omit(daily_date_features_valid_data_test)
daily_date_features_valid_data_test<-daily_date_features_valid_data_test[,1]
daily_date_features_valid_data_test

weekly_date_features_valid_data_test<- tq_get("^FTSE", get = "stock.prices", from = test_startDate, to = test_endDate, complete_cases = FALSE,periodicity='weekly')
weekly_date_features_valid_data_test<-na.omit(weekly_date_features_valid_data_test)
weekly_date_features_valid_data_test<-weekly_date_features_valid_data_test[,1]
weekly_date_features_valid_data_test
####################################################################################################################
##lag data
####################################################################################################################
# we shift our validation price data columns down by one row to avoid data snooping before generating our features
head(features_train_data)
nrow(features_train_data)
class(features_train_data)
length(features_valid_data_real)
na<-c(NA,NA,NA,NA)
features_valid_data_real<-rbind(na,features_valid_data_real[-length(features_valid_data_real),])

features_valid_data_test_real<-rbind(na,features_valid_data_test_real[-length(features_valid_data_test_real),])

################################################################################
#Feature Engineering 
###############################################################################

#Gnerating features including CCI,MACD, OsMA, RSI, STOH and Trend

features<-function(price,p=14){
  Med <- (Hi(price)+Lo(price))/2
  temp <- subset(price[1:5])
  price <- cbind(price,Med)
  #colnames(price) <- c('Open','High','Low','Close','Med')
  
  
  cci<-CCI(price[ ,2:4], n = p)
  macd<-MACD(price[ ,'Med'], 12, 26, 9)[ ,'macd']
  osma<-macd - MACD(price[ ,'Med'],12, 26, 9)[ ,'signal']
  rsi<-RSI(price[ ,'Med'], n = p)
  stoh<-stoch(price[ ,2:4],14, 3, 3)
  xavg<-EMA(price[,4],n=p)
  trend<-price[,4]-xavg;
  
  Input<-cbind(cci, macd, osma, rsi,
               stoh,xavg,trend)
  return(Input)
}

#creating target variable using Zigzag function
trend<-function(price){
  
  zz<-ZigZag(Cl(price), change = 5, percent = T,
             retrace = F, lastExtreme = T)
  ts.plot(zz)
  for(i in 1:length(zz)) { if(is.na(zz[i])) zz[i] = zz[i+1]}
  dz<-c(diff(zz), NA)
  sig<-ifelse(dz>0, 1, ifelse(dz<0, 0, NA))
  
  return(sig)
}

#ploting zigzag line with actual closing prices

zz<-ZigZag(Cl(features_train_data), change = 5, percent = T,
           retrace = F, lastExtreme = T)

ts.plot(features_train_data$Close, col = "red")
lines(zz)
legend("bottomleft", legend=c("Weekly Closing", "Zigzag indicator"),
       col=c("red", "black"),lty = 1)

#plot first difference of zigzag line
ts.plot(diff(zz),ylab= "First Difference of Zigzag", xlab= "Trading weeks")
abline(h=0, col ="blue")


#scaling our features by substracting each of their means and dividing by their standard diviations
total_features_train_data<-(features(features_train_data))

training_remove_NA <- na.omit(features(features_train_data)) #33 NA being removed


Zigzag_train <- trend(features_train_data)[34:nrow(total_features_train_data)]
for(i in 1:length(Zigzag_train)) { if(is.na(Zigzag_train[i])) Zigzag_train[i] = Zigzag_train[i-1]}
training<-cbind(training_remove_NA, Zigzag_train)
preProc<- preProcess(training[,-ncol(training)])


# defind our final variables and response before training the model
X <- predict(preProc, training[,-ncol(training)])

Y <- training[,ncol(training)]


################################################################################################################
# 4 Model Selection
################################################################################################################
########################################################
#validation1
########################################################
#test1
X.train <- array(X,dim=c(1,nrow(X),ncol(X)))
Y.train <- matrix(Y, ncol=length(Y))
learning_rate=0.01
epochs=100
hidden=10

########################################################
#model1
########################################################
model1 <- trainr(X = X.train, Y = Y.train,
                 learningrate = learning_rate, numepochs	= epochs, hidden_dim	= hidden)
total_features_valid_test_data1<-features(features_valid_data_real)
test1 <- na.omit(cbind(total_features_valid_test_data1,ROC(Cl(features_valid_data_real)))) 
X.test1 <- predict(preProc, test1[,-ncol(test1)])
r.test1 <- test1[,ncol(test1)]
X.test1 <- array(X.test1,dim=c(1,nrow(X.test1),ncol(X.test1))) 
Y.pred1 <- predictr(model1, X.test1)
trend.pred1 <- t(ifelse(Y.pred1>.5,1,0)) 
real_trend1 <- trend(features_valid_data_real)
real_trend1<-real_trend1[34:length(real_trend1)]
for(i in 1:length(real_trend1)) { if(is.na(real_trend1[i])) real_trend1[i] = real_trend1[i-1]}
j=0
for( i in 1:(length (real_trend1)-1))
{
  if (real_trend1[i]==trend.pred1[i,]){
    
    j=j+1
  }
}
valid1<-(j/length(trend.pred1))
valid1_compare<-c(model1,valid1)
valid1_compare[30]
########################################################
#model2
################################################################################################################
model2 <- trainr(X = X.train, Y = Y.train,
                 learningrate = learning_rate, numepochs	= epochs, hidden_dim	= hidden)
total_features_valid_test_data2<-features(features_valid_data_real)
test2 <- na.omit(cbind(total_features_valid_test_data2,ROC(Cl(features_valid_data_real)))) 
X.test2 <- predict(preProc, test2[,-ncol(test2)])
r.test2 <- test2[,ncol(test2)]
X.test2 <- array(X.test2,dim=c(1,nrow(X.test2),ncol(X.test2))) 
Y.pred2 <- predictr(model2, X.test2)
trend.pred2 <- t(ifelse(Y.pred2>.5,1,0)) 
real_trend2 <- trend(features_valid_data_real)
real_trend2<-real_trend2[34:length(real_trend2)]
for(i in 1:length(real_trend2)) { if(is.na(real_trend2[i])) real_trend2[i] = real_trend2[i-1]}
j=0
for( i in 1:(length (real_trend2)-1))
{
  if (real_trend2[i]==trend.pred2[i,]){
    
    j=j+1
  }
}
valid2<-(j/length(trend.pred2))
valid2_compare<-c(model2,valid2)
valid2_compare[30]
################################################################################################################
#model3
################################################################################################################
model3 <- trainr(X = X.train, Y = Y.train,
                 learningrate = learning_rate, numepochs	= epochs, hidden_dim	= hidden)
total_features_valid_test_data3<-features(features_valid_data_real)
test3 <- na.omit(cbind(total_features_valid_test_data3,ROC(Cl(features_valid_data_real)))) 
X.test3 <- predict(preProc, test3[,-ncol(test3)])
r.test3 <- test3[,ncol(test3)]
X.test3 <- array(X.test3,dim=c(1,nrow(X.test3),ncol(X.test3))) 
Y.pred3 <- predictr(model3, X.test3)
trend.pred3 <- t(ifelse(Y.pred3>.5,1,0)) 
real_trend3 <- trend(features_valid_data_real)
real_trend3<-real_trend3[34:length(real_trend3)]
for(i in 1:length(real_trend3)) { if(is.na(real_trend3[i])) real_trend3[i] = real_trend3[i-1]}
j=0
for( i in 1:(length (real_trend3)-1))
{
  if (real_trend3[i]==trend.pred3[i,]){
    
    j=j+1
  }
}
valid3<-(j/length(trend.pred3))
valid3_compare<-c(model3,valid3)
valid3_compare[30]
################################################################################################################

################################################################################################################
#model4
########################################################
model4 <- trainr(X = X.train, Y = Y.train,
                 learningrate = learning_rate, numepochs	= epochs, hidden_dim	= hidden)
total_features_valid_test_data4<-features(features_valid_data_real)
test4 <- na.omit(cbind(total_features_valid_test_data4,ROC(Cl(features_valid_data_real)))) 
X.test4 <- predict(preProc, test4[,-ncol(test4)])
r.test4 <- test4[,ncol(test4)]
X.test4 <- array(X.test4,dim=c(1,nrow(X.test4),ncol(X.test4))) 
Y.pred4 <- predictr(model4, X.test4)
trend.pred4 <- t(ifelse(Y.pred4>.5,1,0)) 
real_trend4 <- trend(features_valid_data_real)
real_trend4<-real_trend4[34:length(real_trend4)]
for(i in 1:length(real_trend4)) { if(is.na(real_trend4[i])) real_trend4[i] = real_trend4[i-1]}
j=0
for( i in 1:(length (real_trend4)-1))
{
  if (real_trend4[i]==trend.pred4[i,]){
    
    j=j+1
  }
}
valid4<-(j/length(trend.pred4))
valid4_compare<-c(model4,valid4)
valid4_compare[30]
################################################################################################################
################################################################################################################
#model5
################################################################################################################

model5 <- trainr(X = X.train, Y = Y.train,
                 learningrate = learning_rate, numepochs	= epochs, hidden_dim	= hidden)
total_features_valid_test_data5<-features(features_valid_data_real)
test5 <- na.omit(cbind(total_features_valid_test_data5,ROC(Cl(features_valid_data_real)))) 
X.test5 <- predict(preProc, test5[,-ncol(test5)])
r.test5 <- test5[,ncol(test5)]
X.test5 <- array(X.test5,dim=c(1,nrow(X.test5),ncol(X.test5))) 
Y.pred5 <- predictr(model5, X.test5)
trend.pred5 <- t(ifelse(Y.pred5>.5,1,0)) 
real_trend5 <- trend(features_valid_data_real)
real_trend5<-real_trend5[34:length(real_trend5)]
for(i in 1:length(real_trend5)) { if(is.na(real_trend5[i])) real_trend5[i] = real_trend5[i-1]}
j=0
for( i in 1:(length (real_trend5)-1))
{
  if (real_trend5[i]==trend.pred5[i,]){
    
    j=j+1
  }
}
valid5<-(j/length(trend.pred5))
valid5_compare<-c(model5,valid5)
valid5_compare[30]


valid_total<-c(valid1_compare[30],valid2_compare[30],valid3_compare[30],valid4_compare[30],valid5_compare[30])
valid_total



valid_max<-max(sapply(valid_total, max)) 



if(valid1_compare[30]==valid_max)
{
  best_model<-valid1_compare[-30]
}


if(valid2_compare[30]==valid_max)
{
  best_model<-valid2_compare[-30]
}

if(valid3_compare[30]==valid_max)
{
  best_model<-valid3_compare[-30]
}

if(valid4_compare[30]==valid_max)
{
  best_model<-valid4_compare[-30]
}


if(valid5_compare[30]==valid_max)
{
  best_model<-valid5_compare[-30]
}
best_model



################################################################################################################
#final test
################################################################################################################
total_features_final_test_data<-features(features_valid_data_test_real)

test <- na.omit(cbind(total_features_final_test_data,ROC(Cl(features_valid_data_test_real)))) 
X.test <- predict(preProc, test[,-ncol(test)])
r.test <- test[,ncol(test)]
X.test <- array(X.test,dim=c(1,nrow(X.test),ncol(X.test))) 
Y.pred <- predictr(best_model, X.test)
trend.pred <- t(ifelse(Y.pred>.5,1,0)) 


real_trend <- trend(features_valid_data_test_real)

real_trend<-real_trend[34:length(real_trend)]
for(i in 1:length(real_trend)) { if(is.na(real_trend[i])) real_trend[i] = real_trend[i-1]}

j=0
for( i in 1:(length (real_trend)-1))
{
  if (real_trend[i]==trend.pred[i,]){
    
    j=j+1
  }
}
test<-(j/length(trend.pred))
test



####################SAE<-sae.dnn.train(x= X, y=Y, hidden=c(50,50,50),activationfun = "tanh", 
####################            learningrate = 0.6, momentum = 0.5, learningrate_scale = 1.0,
####################                 output = "sigm", sae_output = "linear",numepochs = 10, batchsize = 100,
####################              hidden_dropout = 0, visible_dropout = 0)
################################################################################################################


weekly_portfolio<-cbind(weekly_date_features_valid_data_test[-nrow(weekly_date_features_valid_data_test),],trend.pred )


daily_date_test<-cbind(daily_date_features_valid_data_test,rep(0,nrow(daily_date_features_valid_data_test)))




for ( i in (1:nrow(weekly_portfolio))){
  ifelse(weekly_portfolio[i,2]==1, weekly_portfolio[i,2]<-0.8,weekly_portfolio[i,2]<-0.2)
  weekly_portfolio
}

weekly_portfolio





i=2
for (j in (1:nrow(daily_date_test))){
  
  ifelse(daily_date_test[j,1]<weekly_portfolio[i,1], daily_date_test[j,2]<- weekly_portfolio[i,2],i<-i+1 )
}


head(weekly_portfolio)
head(daily_date_test)
nrow(daily_date_test)

for (i in (1:nrow(daily_date_test))){
  if(daily_date_test[i,2]==0){
    daily_date_test[i,2]<-daily_date_test[i+1,2]
  }else{daily_date_test[i,2]<-daily_date_test[i,2]}
}

daily_date_test
head(daily_date_test)
########################################################
final_test_startDate <-  test_startDate
endDate <- as.Date("2019-07-10")
tickers <- c("^FTSE")

level_data <- tq_get(tickers, get = "stock.prices", from = final_test_startDate, to = endDate, complete_cases = FALSE)
test_data<-data[(nrow(data)-nrow(level_data)+1):nrow(data),]
test_data<-cbind(level_data[,1],test_data)

level_data<-data.frame(level_data)
test_data<-data.frame(test_data)
weight_frame<- as.xts(level_data[,7], order.by = level_data[,"date"] )

colnames(weight_frame) <- "ftseweight"


oos_weight <- weight_frame

for( i in(1:nrow(oos_weight))){
  oos_weight[i,1]<-daily_date_test[i,2]
}


regime_weight<-oos_weight
for(i in 1:nrow(regime_weight)) { if(is.na(regime_weight[i,])) regime_weight[i] = regime_weight[i-1]}
regime_weight

# add bond weight
regime_weight$bond <- 1 - regime_weight

regime_weight <- na.omit(regime_weight)
colnames(regime_weight) <- c("stock", "bond")
regime_weight_lag1 <- na.omit(lag.xts(regime_weight,1)) # lag to avoid data snooping



##################################################
# get weights for the beginning of week return
tickers1 <- c("^FTSE", "IEF")

level_data <- tq_get(tickers1, get = "stock.prices", from = startDate, to = endDate, complete_cases = FALSE)
level_data <- dcast(level_data, date ~ symbol, value.var='adjusted')

level_data <- as.xts(level_data[,tickers1], order.by = level_data[,"date"] )

asset_rtn <- level_data[, c("^FTSE", "IEF")]
asset_rtn <- asset_rtn[endpoints(level_data, on="weeks"),] 
asset_rtn <- (asset_rtn - lag.xts(asset_rtn))/lag.xts(asset_rtn)
asset_rtn <- asset_rtn[-1,]
asset_rtn <-na.omit(asset_rtn)
# generate benchmark weights
bench_weights <- as.xts( matrix(0.5, nrow= nrow(regime_weight_lag1), ncol = ncol(regime_weight_lag1)), order.by = index(regime_weight_lag1) )
colnames(bench_weights) <- c("stock", "bonds")

########################################################
# 4 - Calculate OOS results
########################################################

# select oos returns
oos_return <- asset_rtn[index(regime_weight_lag1),]

portfolio_returns <- merge( Return.portfolio(oos_return, regime_weight_lag1) ,
                            Return.portfolio(oos_return, bench_weights))

colnames(portfolio_returns) <- c("Regime portfolio", "Buy and hold")

charts.PerformanceSummary(portfolio_returns)
print(rbind(Return.cumulative(portfolio_returns), StdDev.annualized(portfolio_returns), Return.annualized(portfolio_returns), VaR(portfolio_returns, p=0.95)*sqrt(252), maxDrawdown(portfolio_returns), SharpeRatio.annualized(portfolio_returns)))


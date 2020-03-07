####################################################################################################################################################
# - fix time interval and floating time interval 
####################################################################################################################################################
startDate <-  as.Date("1999-01-05") 
endDate <- as.Date("2019-07-10")
tickers <- c("HSBA.L")
HSBC_stock_prices <- tq_get("HSBA.L")

weekly_level_data<- tq_get("HSBA.L", get = "stock.prices", from = startDate, to = endDate, complete_cases = FALSE,periodicity='weekly')
weekly_level_data<-na.omit(weekly_level_data)
daily_level_data<- tq_get("HSBA.L", get = "stock.prices", from = startDate, to = endDate, complete_cases = FALSE)
daily_level_data<-na.omit(daily_level_data)

date_weekly <- weekly_level_data[,1] 
date_daily <- daily_level_data[,1]
level_data_sample <- weekly_level_data[,1:5] # get Open, High, Low, Close Prices

########################################################
# 1.1 - Split Data into 4 PARTS, 
########################################################
length<-nrow(level_data_sample)
period_one_end<-floor((length)/4)
period_two_end<-floor((2*length)/4)
period_three_end<-floor((3*length)/4)
period_four_end<-length

date<-c(period_one_end,period_two_end,period_three_end,period_four_end)
date

part_one_level_data_sample<-level_data_sample[1:period_one_end,]
part_two_level_data_sample<-level_data_sample[period_one_end:period_two_end,]
part_three_level_data_sample<-level_data_sample[period_two_end:period_three_end,]
part_four_level_data_sample<-level_data_sample[period_three_end:period_four_end,]
part_one_length<-nrow(part_one_level_data_sample)
part_two_length<-nrow(part_two_level_data_sample)
part_three_length<-nrow(part_three_level_data_sample)
part_four_length<-nrow(part_four_level_data_sample)
total_length<-c(part_one_length,part_two_length,part_three_length,part_four_length)
total_length
########################################################
# 1.2 - Split Data into 4 PARTS, part1
########################################################
part_two_train_date_end<-floor((part_two_length)/2)
part_two_validation1_date_end<-floor((3*part_two_length)/4)
part_two_date<-c(part_two_train_date_end,part_two_validation1_date_end)
part_two_date
# get weekly data from part1 for training model
part_two_features_train_data <- part_two_level_data_sample[1:part_two_train_date_end,] 
colnames(part_two_features_train_data) <- c('Date','Open','High','Low','Close')

# get weekly data from 2012/01/02-2013/12/30 for validating model 
part_two_features_valid_data_real<- part_two_level_data_sample[(part_two_train_date_end-32):part_two_validation1_date_end,] # need to burn 33 more data points until the first feature indicator
part_two_valid_data <- part_two_level_data_sample[(part_two_train_date_end+1):part_two_validation1_date_end,]              #actual valid period                                          
colnames(part_two_features_valid_data_real) <- c('Date','Open','High','Low','Close')


# get weekly data from 2016/01/04-2019/01/01 for testing
part_two_features_valid_data_test_real<-part_two_level_data_sample[(part_two_validation1_date_end-32):part_two_length,] #need to burn 33 more data points until the first feature indicator
part_two_valid_data_test<- part_two_level_data_sample[(part_two_validation1_date_end+1):part_two_length,]        # actual test period
colnames(part_two_features_valid_data_test_real) <- c('Date','Open','High','Low','Close')

part_two_Date_start<-part_two_level_data_sample[part_two_validation1_date_end+1,1]
part_two_Date_start<-data.frame(part_two_Date_start)
part_two_test_startDate<-as.Date(part_two_Date_start[1,1])

part_two_Date_end<-part_two_level_data_sample[part_two_length,1]
part_two_Date_end<-data.frame(part_two_Date_end)
part_two_test_endDate<-as.Date(part_two_Date_end[1,1])

part_two_daily_date_features_valid_data_test<- tq_get("HSBA.L", get = "stock.prices", from = part_two_test_startDate, to = part_two_test_endDate, complete_cases = FALSE)
part_two_daily_date_features_valid_data_test<-na.omit(part_two_daily_date_features_valid_data_test)
part_two_daily_date_features_valid_data_test<-part_two_daily_date_features_valid_data_test[,1]
part_two_daily_date_features_valid_data_test


part_two_weekly_date_features_valid_data_test<- tq_get("HSBA.L", get = "stock.prices", from = part_two_test_startDate, to = part_two_test_endDate, complete_cases = FALSE,periodicity='weekly')
part_two_weekly_date_features_valid_data_test<-na.omit(part_two_weekly_date_features_valid_data_test)
part_two_weekly_date_features_valid_data_test<-part_two_weekly_date_features_valid_data_test[,1]
part_two_weekly_date_features_valid_data_test
####################################################################################################################
##lag data
####################################################################################################################

# we shift our validation price data columns down by two row to avoid data snooping before generating our features

head(part_two_features_train_data)
nrow(part_two_features_train_data)
class(part_two_features_train_data)


nrow(part_two_features_valid_data_real)
na<-c(NA,NA,NA,NA)
part_two_features_valid_data_real<-rbind(na,part_two_features_valid_data_real[-length(part_two_features_valid_data_real),])

part_two_features_valid_data_test_real<-rbind(na,part_two_features_valid_data_test_real[-length(part_two_features_valid_data_test_real),])

################################################################################
#1.3Feature Engineering 
###############################################################################

#Gnerating features including CCI,MACD, OsMA, RSI, STOH and Trend

features<-function(price,p=14){
  Med <- (Hi(price)+Lo(price))/2
  temp <- subset(price[1:5])
  price <- cbind(price,Med)
  colnames(price) <- c('Open','High','Low','Close','Med')
  
  
  cci<-CCI(price[ ,3:5], n = p)
  macd<-MACD(price[ ,'Med'], 12, 26, 9)[ ,'macd']
  osma<-macd - MACD(price[ ,'Med'],12, 26, 9)[ ,'signal']
  rsi<-RSI(price[ ,'Med'], n = p)
  stoh<-stoch(price[ ,3:5],14, 3, 3)
  xavg<-EMA(price[,5],n=p)
  trend<-price[,5]-xavg;
  
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

zz<-ZigZag(Cl(part_two_features_train_data), change = 5, percent = T,
           retrace = F, lastExtreme = T)

ts.plot(part_two_features_train_data$Close, main = "2002/07/29--2010/12/27", ylab= "S&P500 Closing Prices", xlab= "Trading weeks", col = "red")
lines(zz)
legend("bottomleft", legend=c("Weekly Closing", "Zigzag indicator"),
       col=c("red", "black"),lty = 1)

#plot first difference of zigzag line
ts.plot(diff(zz),ylab= "First Difference of Zigzag", xlab= "Trading weeks" , main = "2002/07/29--2010/12/27")
abline(h=0, col ="blue")


#scaling our features by substracting each of their means and dividing by their standard diviations
part_two_total_features_train_data<-(features(part_two_features_train_data))

part_two_training_remove_NA <- na.omit(features(part_two_features_train_data)) #33 NA being removed


part_two_Zigzag_train <- trend(part_two_features_train_data)[34:nrow(part_two_total_features_train_data)]
for(i in 1:length(part_two_Zigzag_train)) { if(is.na(part_two_Zigzag_train[i])) part_two_Zigzag_train[i] = part_two_Zigzag_train[i-1]}
part_two_training<-cbind(part_two_training_remove_NA, part_two_Zigzag_train)
part_two_preProc<- preProcess(part_two_training[,-ncol(part_two_training)])


# defind our final variables and response before training the model
part_two_X <- predict(part_two_preProc, part_two_training[,-ncol(part_two_training)])

part_two_Y <- part_two_training[,ncol(part_two_training)]


################################################################################################################
# 1.4 Model Selection
################################################################################################################
########################################################
#validation1
########################################################
#test1
part_two_X.train <- array(part_two_X,dim=c(1,nrow(part_two_X),ncol(part_two_X)))
part_two_Y.train <- matrix(part_two_Y, ncol=length(part_two_Y))


learning_rate=0.01
epochs=100
hidden=10

########################################################
#model1
########################################################
part_two_model1 <- trainr(X = part_two_X.train, Y = part_two_Y.train,
                          learningrate = learning_rate, numepochs	= epochs, hidden_dim	= hidden)
part_two_total_features_valid_test_data1<-features(part_two_features_valid_data_real)
part_two_test1 <- na.omit(cbind(part_two_total_features_valid_test_data1,ROC(Cl(part_two_features_valid_data_real)))) 
part_two_X.test1 <- predict(part_two_preProc, part_two_test1[,-ncol(part_two_test1)])
part_two_r.test1 <- part_two_test1[,ncol(part_two_test1)]
part_two_X.test1 <- array(part_two_X.test1,dim=c(1,nrow(part_two_X.test1),ncol(part_two_X.test1))) 
part_two_Y.pred1 <- predictr(part_two_model1, part_two_X.test1)
part_two_trend.pred1 <- t(ifelse(part_two_Y.pred1>.5,1,0)) 
part_two_real_trend1 <- trend(part_two_features_valid_data_real)
part_two_real_trend1<-part_two_real_trend1[34:length(part_two_real_trend1)]
for(i in 1:length(part_two_real_trend1)) { if(is.na(part_two_real_trend1[i])) part_two_real_trend1[i] = part_two_real_trend1[i-1]}
j=0
for( i in 1:(length (part_two_real_trend1)-1))
{
  if (part_two_real_trend1[i]==part_two_trend.pred1[i,]){
    
    j=j+1
  }
}
part_two_valid1<-(j/length(part_two_trend.pred1))
part_two_valid1_compare<-c(part_two_model1,part_two_valid1)
part_two_valid1_compare[30]

########################################################
#model2
################################################################################################################
part_two_model2 <- trainr(X = part_two_X.train, Y = part_two_Y.train,
                          learningrate = learning_rate, numepochs	= epochs, hidden_dim	= hidden)
part_two_total_features_valid_test_data2<-features(part_two_features_valid_data_real)
part_two_test2 <- na.omit(cbind(part_two_total_features_valid_test_data2,ROC(Cl(part_two_features_valid_data_real)))) 
part_two_X.test2 <- predict(part_two_preProc, part_two_test2[,-ncol(part_two_test2)])
part_two_r.test2 <- part_two_test2[,ncol(part_two_test2)]
part_two_X.test2 <- array(part_two_X.test2,dim=c(1,nrow(part_two_X.test2),ncol(part_two_X.test2))) 
part_two_Y.pred2 <- predictr(part_two_model2, part_two_X.test2)
part_two_trend.pred2 <- t(ifelse(part_two_Y.pred2>.5,1,0)) 
part_two_real_trend2 <- trend(part_two_features_valid_data_real)
part_two_real_trend2<-part_two_real_trend2[34:length(part_two_real_trend2)]
for(i in 1:length(part_two_real_trend2)) { if(is.na(part_two_real_trend2[i])) part_two_real_trend2[i] = part_two_real_trend2[i-1]}
j=0
for( i in 1:(length (part_two_real_trend2)-1))
{
  if (part_two_real_trend2[i]==part_two_trend.pred2[i,]){
    
    j=j+1
  }
}
part_two_valid2<-(j/length(part_two_trend.pred2))
part_two_valid2_compare<-c(part_two_model2,part_two_valid2)
part_two_valid2_compare[30]

################################################################################################################
#model3
################################################################################################################
part_two_model3 <- trainr(X = part_two_X.train, Y = part_two_Y.train,
                          learningrate = learning_rate, numepochs	= epochs, hidden_dim	= hidden)
part_two_total_features_valid_test_data3<-features(part_two_features_valid_data_real)
part_two_test3 <- na.omit(cbind(part_two_total_features_valid_test_data3,ROC(Cl(part_two_features_valid_data_real)))) 
part_two_X.test3 <- predict(part_two_preProc, part_two_test3[,-ncol(part_two_test3)])
part_two_r.test3 <- part_two_test3[,ncol(part_two_test3)]
part_two_X.test3 <- array(part_two_X.test3,dim=c(1,nrow(part_two_X.test3),ncol(part_two_X.test3))) 
part_two_Y.pred3 <- predictr(part_two_model3, part_two_X.test3)
part_two_trend.pred3 <- t(ifelse(part_two_Y.pred3>.5,1,0)) 
part_two_real_trend3 <- trend(part_two_features_valid_data_real)
part_two_real_trend3<-part_two_real_trend3[34:length(part_two_real_trend3)]
for(i in 1:length(part_two_real_trend3)) { if(is.na(part_two_real_trend3[i])) part_two_real_trend3[i] = part_two_real_trend3[i-1]}
j=0
for( i in 1:(length (part_two_real_trend3)-1))
{
  if (part_two_real_trend3[i]==part_two_trend.pred3[i,]){
    
    j=j+1
  }
}
part_two_valid3<-(j/length(part_two_trend.pred3))
part_two_valid3_compare<-c(part_two_model3,part_two_valid3)
part_two_valid3_compare[30]
################################################################################################################

################################################################################################################
#model4
########################################################
part_two_model4 <- trainr(X = part_two_X.train, Y = part_two_Y.train,
                          learningrate = learning_rate, numepochs	= epochs, hidden_dim	= hidden)
part_two_total_features_valid_test_data4<-features(part_two_features_valid_data_real)
part_two_test4 <- na.omit(cbind(part_two_total_features_valid_test_data4,ROC(Cl(part_two_features_valid_data_real)))) 
part_two_X.test4 <- predict(part_two_preProc, part_two_test4[,-ncol(part_two_test4)])
part_two_r.test4 <- part_two_test4[,ncol(part_two_test4)]
part_two_X.test4 <- array(part_two_X.test4,dim=c(1,nrow(part_two_X.test4),ncol(part_two_X.test4))) 
part_two_Y.pred4 <- predictr(part_two_model4, part_two_X.test4)
part_two_trend.pred4 <- t(ifelse(part_two_Y.pred4>.5,1,0)) 
part_two_real_trend4 <- trend(part_two_features_valid_data_real)
part_two_real_trend4<-part_two_real_trend4[34:length(part_two_real_trend4)]
for(i in 1:length(part_two_real_trend4)) { if(is.na(part_two_real_trend4[i])) part_two_real_trend4[i] = part_two_real_trend4[i-1]}
j=0
for( i in 1:(length (part_two_real_trend4)-1))
{
  if (part_two_real_trend4[i]==part_two_trend.pred4[i,]){
    
    j=j+1
  }
}
part_two_valid4<-(j/length(part_two_trend.pred4))
part_two_valid4_compare<-c(part_two_model4,part_two_valid4)
part_two_valid4_compare[30]
################################################################################################################
################################################################################################################
#model5
################################################################################################################

part_two_model5 <- trainr(X = part_two_X.train, Y = part_two_Y.train,
                          learningrate = learning_rate, numepochs	= epochs, hidden_dim	= hidden)
part_two_total_features_valid_test_data5<-features(part_two_features_valid_data_real)
part_two_test5 <- na.omit(cbind(part_two_total_features_valid_test_data5,ROC(Cl(part_two_features_valid_data_real)))) 
part_two_X.test5 <- predict(part_two_preProc, part_two_test5[,-ncol(part_two_test5)])
part_two_r.test5 <- part_two_test5[,ncol(part_two_test5)]
part_two_X.test5 <- array(part_two_X.test5,dim=c(1,nrow(part_two_X.test5),ncol(part_two_X.test5))) 
part_two_Y.pred5 <- predictr(part_two_model5, part_two_X.test5)
part_two_trend.pred5 <- t(ifelse(part_two_Y.pred5>.5,1,0)) 
part_two_real_trend5 <- trend(part_two_features_valid_data_real)
part_two_real_trend5<-part_two_real_trend5[34:length(part_two_real_trend5)]
for(i in 1:length(part_two_real_trend5)) { if(is.na(part_two_real_trend5[i])) part_two_real_trend5[i] = part_two_real_trend5[i-1]}
j=0
for( i in 1:(length (part_two_real_trend5)-1))
{
  if (part_two_real_trend5[i]==part_two_trend.pred5[i,]){
    
    j=j+1
  }
}
part_two_valid5<-(j/length(part_two_trend.pred5))
part_two_valid5_compare<-c(part_two_model5,part_two_valid5)
part_two_valid5_compare[30]


part_two_valid_total<-c(part_two_valid1_compare[30],part_two_valid2_compare[30],part_two_valid3_compare[30],part_two_valid4_compare[30],part_two_valid5_compare[30])
part_two_valid_total



part_two_valid_max<-max(sapply(part_two_valid_total, max)) 



if(part_two_valid1_compare[30]==part_two_valid_max)
{
  part_two_best_model<-part_two_valid1_compare[-30]
}


if(part_two_valid2_compare[30]==part_two_valid_max)
{
  part_two_best_model<-part_two_valid2_compare[-30]
}

if(part_two_valid3_compare[30]==part_two_valid_max)
{
  part_two_best_model<-part_two_valid3_compare[-30]
}

if(part_two_valid4_compare[30]==part_two_valid_max)
{
  part_two_best_model<-part_two_valid4_compare[-30]
}


if(part_two_valid5_compare[30]==part_two_valid_max)
{
  part_two_best_model<-part_two_valid5_compare[-30]
}
part_two_best_model



################################################################################################################
#final test
################################################################################################################
part_two_total_features_final_test_data<-features(part_two_features_valid_data_test_real)

part_two_test <- na.omit(cbind(part_two_total_features_final_test_data,ROC(Cl(part_two_features_valid_data_test_real)))) 
part_two_X.test <- predict(part_two_preProc, part_two_test[,-ncol(part_two_test)])
part_two_r.test <- part_two_test[,ncol(part_two_test)]
part_two_X.test <- array(part_two_X.test,dim=c(1,nrow(part_two_X.test),ncol(part_two_X.test))) 
part_two_Y.pred <- predictr(part_two_best_model, part_two_X.test)
part_two_trend.pred <- t(ifelse(part_two_Y.pred>.5,1,0)) 


part_two_real_trend <- trend(part_two_features_valid_data_test_real)

part_two_real_trend<-part_two_real_trend[34:length(part_two_real_trend)]
for(i in 1:length(part_two_real_trend)) { if(is.na(part_two_real_trend[i])) part_two_real_trend[i] = part_two_real_trend[i-1]}

j=0
for( i in 1:(length (part_two_real_trend)-1))
{
  if (part_two_real_trend[i]==part_two_trend.pred[i,]){
    
    j=j+1
  }
}
part_two_test<-(j/length(part_two_trend.pred))
part_two_test


test_result<-c(part_two_valid_max,part_two_test)
####################SAE<-sae.dnn.train(x= X, y=Y, hidden=c(50,50,50),activationfun = "tanh", 
####################            learningrate = 0.6, momentum = 0.5, learningrate_scale = 1.0,
####################                 output = "sigm", sae_output = "linear",numepochs = 10, batchsize = 100,
####################              hidden_dropout = 0, visible_dropout = 0)
################################################################################################################
part_two_weekly_portfolio<-cbind(part_two_weekly_date_features_valid_data_test[-nrow(part_two_weekly_date_features_valid_data_test),],part_two_trend.pred)
part_two_daily_date_test<-cbind(part_two_daily_date_features_valid_data_test,rep(0,nrow(part_two_daily_date_features_valid_data_test)))

for ( i in (1:nrow(part_two_weekly_portfolio))){
  ifelse(part_two_weekly_portfolio[i,2]==1, part_two_weekly_portfolio[i,2]<-0.8,part_two_weekly_portfolio[i,2]<-0.2)
  part_two_weekly_portfolio
}

part_two_weekly_portfolio





i=2
for (j in (1:nrow(part_two_daily_date_test))){
  
  ifelse(part_two_daily_date_test[j,1]<part_two_weekly_portfolio[i,1], part_two_daily_date_test[j,2]<- part_two_weekly_portfolio[i,2],i<-i+1 )
}


head(part_two_weekly_portfolio)
head(part_two_daily_date_test)
nrow(part_two_daily_date_test)

for (i in (1:nrow(part_two_daily_date_test))){
  if(part_two_daily_date_test[i,2]==0){
    part_two_daily_date_test[i,2]<-part_two_daily_date_test[i+1,2]
  }else{part_two_daily_date_test[i,2]<-part_two_daily_date_test[i,2]}
}

part_two_daily_date_test
head(part_two_daily_date_test)
########################################################
part_two_final_test_startDate <-  part_two_test_startDate

tickers <- c("HSBA.L")

part_two_level_data <- tq_get(tickers, get = "stock.prices", from = part_two_final_test_startDate, to = part_two_test_endDate, complete_cases = FALSE)

part_two_level_data<-data.frame(part_two_level_data)
part_two_weight_frame<- as.xts(part_two_level_data[,7], order.by = part_two_level_data[,"date"] )

colnames(part_two_weight_frame) <- "SP500weight"


part_two_oos_weight <- part_two_weight_frame

for( i in(1:nrow(part_two_oos_weight))){
  part_two_oos_weight[i,1]<-part_two_daily_date_test[i,2]
}


part_two_regime_weight<-part_two_oos_weight
for(i in 1:nrow(part_two_regime_weight)) { if(is.na(part_two_regime_weight[i,])) part_two_regime_weight[i] = part_two_regime_weight[i-1]}
part_two_regime_weight

# add bond weight
part_two_regime_weight$bond <- 1 - part_two_regime_weight

part_two_regime_weight <- na.omit(part_two_regime_weight)
colnames(part_two_regime_weight) <- c("stock", "bond")
part_two_regime_weight_lag1 <- na.omit(lag.xts(part_two_regime_weight,1)) # lag to avoid data snooping

##################################################
# get weights for the beginning of week return
tickers1 <- c("HSBA.L", "IEF")

part_two_level_data <- tq_get(tickers1, get = "stock.prices", from = part_two_final_test_startDate, to = part_two_test_endDate, complete_cases = FALSE)
part_two_level_data <- dcast(part_two_level_data, date ~ symbol, value.var='adjusted')

part_two_level_data <- as.xts(part_two_level_data[,tickers1], order.by = part_two_level_data[,"date"] )

part_two_asset_rtn <- part_two_level_data[, c("HSBA.L", "IEF")]
part_two_asset_rtn <- part_two_asset_rtn[endpoints(part_two_level_data, on="weeks"),] 
part_two_asset_rtn <- (part_two_asset_rtn - lag.xts(part_two_asset_rtn))/lag.xts(part_two_asset_rtn)
part_two_asset_rtn <- part_two_asset_rtn[-1,]
part_two_asset_rtn <-na.omit(part_two_asset_rtn)
# generate benchmark weights
part_two_bench_weights <- as.xts( matrix(0.5, nrow= nrow(part_two_regime_weight_lag1), ncol = ncol(part_two_regime_weight_lag1)), order.by = index(part_two_regime_weight_lag1) )
colnames(part_two_bench_weights) <- c("stock", "bonds")

########################################################
# 1.5 - Calculate OOS results
########################################################

# select oos returns
part_two_oos_return <-part_two_asset_rtn[index(part_two_regime_weight_lag1),]

part_two_portfolio_returns <- merge( Return.portfolio(part_two_oos_return, part_two_regime_weight_lag1) ,
                                     Return.portfolio(part_two_oos_return, part_two_bench_weights))

colnames(part_two_portfolio_returns) <- c("Regime portfolio", "Buy and hold")

charts.PerformanceSummary(part_two_portfolio_returns)
print(rbind(Return.cumulative(part_two_portfolio_returns), StdDev.annualized(part_two_portfolio_returns), Return.annualized(part_two_portfolio_returns), VaR(part_two_portfolio_returns, p=0.95)*sqrt(252), maxDrawdown(part_two_portfolio_returns), SharpeRatio.annualized(part_two_portfolio_returns)))
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################



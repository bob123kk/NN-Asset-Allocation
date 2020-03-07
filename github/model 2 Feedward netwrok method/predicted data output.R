#predicted data output
library(xts)
data=read.csv("daxpart4.csv", header = T)##file
test_start=which(data$Date=="2018/3/26")
train_end=test_start-1
data= xts(data[,-1],as.Date(data$Date, format="%Y/%m/%d"))
data=data$dax
data_train=data[1:train_end]
data_pred=read.csv("daxpart4_pred.csv",header = F)##prediction file
data_pred=t(data_pred)
length(data_pred)
data_pred=data_pred*(max(data_train)-min(data_train))+min(data_train)
data_truth=data[(length(data)-length(data_pred)+1):length(data)]
data_new=cbind.xts(data_truth,data_pred)
write.zoo(data_new,sep=",",file="/Users/zozozoe/Desktop/pred_daxpart4.csv")##new file



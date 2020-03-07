data=read.csv("ftse.csv",header=T)
data1=data[complete.cases(data),]
which(hsbc_time$Date=="2017/8/14") ##test start
##output data with time
write.csv(data1,file="/Users/zozozoe/Desktop/ftsetime.csv")

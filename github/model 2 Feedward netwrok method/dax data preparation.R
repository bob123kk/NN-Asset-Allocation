#DAX data preparation

library(xts)
dax=read.csv("daxtime.csv", header = T)
#5 year
train_start5=which(dax$Date=="2014/1/6")#train_start
dax5=dax[train_start5:5199,]
which(dax5$Date=="2017/8/14")#test_start
write.csv(dax5,file="/Users/zozozoe/Desktop/dax5.csv")

#10 year
train_start10=which(dax$Date=="2009/1/5")#train_start
dax10=dax[train_start10:5199,]
which(dax10$Date=="2017/8/14")#test_start
write.csv(dax10,file="/Users/zozozoe/Desktop/dax10.csv")

#15 year
train_start15=which(dax$Date=="2004/1/5")#train_start
dax15=dax[train_start15:5199,]
which(dax15$Date=="2017/8/14")#test_start
write.csv(dax15,file="/Users/zozozoe/Desktop/dax15.csv")

#20 year: same as original dataset

#fix and floating
#part1 1999-2004
train_start1=which(dax$Date=="1999/1/4")#train_start
test_end1=which(dax$Date=="2004/2/9")
daxpart1=dax[train_start1:test_end1,]
which(daxpart1$Date=="2002/11/4")#test_start
write.csv(daxpart1,file="/Users/zozozoe/Desktop/daxpart1.csv")

#part2 2004-2009
train_start2=which(dax$Date=="2004/2/9")#train_start
test_end2=which(dax$Date=="2009/3/30")
daxpart2=dax[train_start2:test_end2,]
which(daxpart2$Date=="2007/12/17")#test_start
write.csv(daxpart2,file="/Users/zozozoe/Desktop/daxpart2.csv")

#part3 2009-2014
train_start3=which(dax$Date=="2009/3/30")#train_start
test_end3=which(dax$Date=="2014/5/19")
daxpart3=dax[train_start3:test_end3,]
which(daxpart3$Date=="2013/2/4")#test_start
write.csv(daxpart3,file="/Users/zozozoe/Desktop/daxpart3.csv")

#part4 2014-2019
train_start4=which(dax$Date=="2014/5/19")#train_start
test_end4=which(dax$Date=="2019/7/10")
daxpart4=dax[train_start4:test_end4,]
which(daxpart4$Date=="2018/3/26")#test_start
write.csv(daxpart4,file="/Users/zozozoe/Desktop/daxpart4.csv")


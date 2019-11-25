rm(list = ls())
setwd("D:/时间序列分析/习题数据、案例数据、R代码/")
library(readxl)
#生成向量，并转为时间序列数据
dat1 <- c(10,20,30,40,50,60,70,80,99,89,79,69)
dat1
dat1_ts <- ts(dat1,frequency = 4,start = c(2015,1))
dat1_ts
dat2 <- as.data.frame(read.csv("GDP.csv"))
#提取前30年数据，生成gdpb78.csv文件
gdpb78 <- dat2[1:30,]
write.csv(gdpb78,"gdpb78.csv")
#去掉前3年的缺失值miss value，生成gdpB.csv文件
gdpB <- na.omit(dat2)#将有缺失值的第一年去掉
write.csv(gdpB,"gdpB.csv")
#取Year、GDP、HR三列，生成gdp4.csv文件
gdp4 <- dat2[,c("Year","GDP","HR")]
write.csv(gdp4,"gdp4.csv")
#取1978年后数据，生成gdpa78.csv文件
gdpa78 <- subset(dat2,dat2$Year>1978)
write.csv(gdpa78,"gdpa78.csv")

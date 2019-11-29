#unit4

#例4-1
#读入数据
x<-c(8444,9215,8879,8990,8115,9457,8590,9294,8997,9574,9051,9724,9120,
     + 10143,9746,10074,9578,10817,10116,10779,9901,11266,10686,10961,10121,
     + 11333,10677,11325,10698,11624,11502,11393,10609,12077,11376,11777,
     + 11225,12231,11884,12109)
#构造时间变量
t<-c(1:40)
#拟合回归模型
x.fit<-lm(x~t)
#查看拟合信息
summary(x.fit)
#绘制拟合效果图
x<-ts(x)
plot(x)
abline(lm(x~t),col=2)

#例4-2
#读入数据
a<-read.table("E:/R/data/file12.csv",sep=",",header = T)
x<-ts(a$output,start=1949)
#lm函数拟合
t1<-c(1:60)
t2<-t1^2
x.fit1<-lm(x~t1+t2)
summary(x.fit1)
#nls函数拟合
x.fit2<-nls(x~a+b*t1+c*t1^2,start = list(a=1,b=1,c=1))
summary(x.fit2)
y<-predict(x.fit2)
y<-ts(y,start = 1949)
plot(x,type = "p")
lines(y,col=2,lwd=2)

#例4-4
library(TTR)
a<-read.table("E:/R/data/file6.csv",",",header = T)
x<-ts(a$temp,start = 1949)
x.ma<-SMA(x,n=5)
plot(x,type = "o")
lines(x.ma,col=2,lwd=2)

#例4-5
#读入序列
a<-read.table("E:/R/data/file4.csv",sep=",",header = T)
x<-ts(a$output,start = 1964)
#进行Holt两参数平滑
x.fit<-HoltWinters(x,gamma = F)
x.fit
#绘制Holt两参数指数平滑拟合效果图
plot(x.fit)
#预测序列并绘制预测效果图
x.fore<-forecast(x.fit,h=10)
x.fore
plot(x.fore)

#例4-6
#读入序列
b<-read.table("E:/R/data/file5.csv",sep=",",header = T)
x<-ts(b$milk,start = c(1962,1),frequency = 12)
#进行Holt-winters三参数指数平滑
x.fit<-HoltWinters(x)
x.fit
#绘制Holt-winters三参数指数平滑拟合效果图
plot(x.fit)
#预测序列并绘制预测效果图
x.fore<-forecast(x.fit,h=24)
plot(x.fore)

#例4-8
#读入序列,并绘制时序图
c<-read.table("E:/R/data/file14.csv",sep=",",header = T)
x<-ts(c$sales,start = c(1993,1),frequency = 12)
plot(x)
#确定性因素分解
x.fit<-decompose(x,type="mult")
#查看季节指数，并绘制季节指数图
x.fit$figure
plot(x.fit$figure,type = "o")
#查看趋势拟合值，并绘制趋势拟合图
x.fit$trend
plot(x.fit$trend)
#查看随机波动(残差)值，并绘制残差图
x.fit$random
plot(x.fit$random)
#命令集合
x.fit
plot(x.fit)


#unit3

#例3-1
x1<-arima.sim(n=100,list(ar=0.8))
x3<-arima.sim(n=100,list(ar=c(1,-0.5)))
e<-rnorm(100)
x2<-filter(e,filter = -1.1,method = "recursive")
x4<-filter(e,filter = c(1,0.5),method = "recursive")
ts.plot(x1)
ts.plot(x2)
ts.plot(x3)
ts.plot(x4)

#例3-5
x1<-arima.sim(n=1000,list(ar=0.8))
x2<-arima.sim(n=1000,list(ar=-0.8))
x3<-arima.sim(n=1000,list(ar=c(1,-0.5)))
x4<-arima.sim(n=1000,list(ar=c(-1,-0.5)))
acf(x1)
acf(x2)
acf(x3)
acf(x4)

#例3-5 续
pacf(x1)
pacf(x2)
pacf(x3)
pacf(x4)

#例3-6
x1<-arima.sim(n=1000,list(ma=-2))
x2<-arima.sim(n=1000,list(ma=-0.5))
x3<-arima.sim(n=1000,list(ma=c(-4/5,16/25)))
x4<-arima.sim(n=1000,list(ar=c(-5/4,25/16)))
acf(x1)
acf(x2)
acf(x3)
acf(x4)

#例3-6 续(2)
pacf(x1)
pacf(x2)
pacf(x3)
pacf(x4)

#例3-8
x<-arima.sim(n=1000,list(ar=0.5,ma=-0.8))
acf(x)
pacf(x)

#例3-9
#读入数据，并绘制时序图
a<-read.table("E:/R/data/file8.csv",sep=",",header = T)
x<-ts(a$kilometer,start = 1950)
plot(x)
#白噪声检验
for(i in 1:2) print(Box.test(x,type = "Ljung-Box",lag=6*i))
#绘制自相关图和偏自相关图
acf(x)
pacf(x)

#例3-10
#读入数据，并绘制时序图
overshort<-read.table("E:/R/data/file9.csv",sep=",",header = T)
overshort<-ts(overshort)
plot(overshort)
#白噪声检验
for(i in 1:2) print(Box.test(overshort,type = "Ljung-Box",lag=6*i))
#绘制自相关图和偏自相关图
acf(overshort)
pacf(overshort)

#例3-11
#读入数据，并绘制时序图
b<-read.table("E:/R/data/file10.csv",sep=",",header = T)
dif_x<-ts(diff(b$change_temp),start = 1880)
plot(dif_x)
#白噪声检验
for(i in 1:2) print(Box.test(dif_x,type = "Ljung-Box",lag=6*i))
#绘制自相关图和偏自相关图
acf(dif_x)
pacf(dif_x)

library(zoo)
library(forecast)
#例3-9系统自动定阶
auto.arima(x)
#例3-10系统自动定阶
auto.arima(overshort)
#例3-11系统自动定阶
auto.arima(dif_x)

#例3-9续(1)
a<-read.table("E:/R/data/file8.csv",sep=",",header = T)
x<-ts(a$kilometer,start=1950)
x.fit<-arima(x,order = c(2,0,0),method = "ML")
x.fit

#例3-10续(1)
overshort<-read.table("E:/R/data/file9.csv",sep=",",header = T)
overshort<-ts(overshort)
overshort.fit<-arima(overshort,order = c(0,0,1))
overshort.fit

#例3-11续(1)
b<-read.table("E:/R/data/file10.csv",sep=",",header = T)
dif_x<-ts(diff(b$chang_temp),start = 1880)
dif_x.fit<-arima(dif_x,order = c(1,0,1))
dif_x.fit

#例3-9续(2)
a<-read.table("E:/R/data/file8.csv",sep=",",header = T)
x<-ts(a$kilometer,start=1950)
x.fit<-arima(x,order = c(2,0,0),method = "ML")
for(i in 1:2) print(Box.test(x.fit$residual,lag=6*i))

#例3-10续(2)
overshort<-read.table("E:/R/data/file9.csv",sep=",",header = T)
overshort<-ts(overshort)
overshort.fit<-arima(overshort,order = c(0,0,1))
for(i in 1:2) print(Box.test(overshort.fit$residual,lag=6*i))

#例3-11续(2)
b<-read.table("E:/R/data/file10.csv",sep=",",header = T)
dif_x<-ts(diff(b$chang_temp),start = 1880)
dif_x.fit<-arima(dif_x,order = c(1,0,1),method = "CSS")
for(i in 1:2) print(Box.test(dif_x.fit$residual,lag=6*i))

#例3-9续(3)
a<-read.table("E:/R/data/file8.csv",sep=",",header = T)
x<-ts(a$kilometer,start=1950)
x.fit<-arima(x,order = c(2,0,0),method = "ML")
x.fit
#ar1系数显著性检验
t1<-0.7185/0.1083
pt(t1,df=56,lower.tail = F)
#ar2系数显著性检验
t2<-0.5294/0.1067
pt(t2,df=56,lower.tail = T)
#ar3系数显著性检验
t0=11.0223/3.0906
pt(t0,df=56,lower.tail = F)

#例3-15
#读入数据，绘制时序图
x<-read.table(file = "E:/R/data/file11.csv",sep=",",header = T)
x<-ts(x)
plot(x)
#序列白噪声检验
for(i in 1:2) print(Box.test(x,lag=6*i))
#绘制自相关图和偏自相关图
acf(x)
pacf(x)
#拟合MA(2)模型
x.fit1<-arima(x,order = c(0,0,2))
x.fit1
#MA(2)模型显著性检验
for(i in 1:2) print(Box.test(x.fit1$residual,lag=6*i))
#拟合AR(1)模型
x.fit2<-arima(x,order = c(1,0,0))
x.fit2
#AR(1)模型显著性检验
for(i in 1:2) print(Box.test(x.fit2$residual,lag=6*i))

#例3-9续(4)
a<-read.table("E:/R/data/file8.csv",sep=",",header = T)
x<-ts(a$kilometer,start=1950)
x.fit<-arima(x,order = c(2,0,0))
x.fore<-forecast(x.fit,h=5)
x.fore
#系统默认输出预测图
plot(x.fore)
#个性化输出预测图
L1<-x.fore$fitted-1.96*sqrt(x.fit$sigma2)
U1<-x.fore$fitted+1.96*sqrt(x.fit$sigma2)
L2<-ts(x.fore$lower[,2],start = 2009)
U2<-ts(x.fore$upper[,2],start = 2009)
c1<-min(x,L1,L2)
c2<-max(x,L2,U2)
plot(x,type = "p",pch=8,xlim = c(1950,2013),ylim = c(c1,c2))
lines(x.fore$fitted,col=2,lwd=2)
lines(x.fore$mean,col=2,lwd=2)
lines(L1,col=4,lty=2)
lines(L2,col=4,lty=2)
lines(U1,col=4,lty=2)
lines(U2,col=4,lty=2)

#第一题 
plot(lh)
x<-lh
#n=5，前置移动平均
library(TTR)
SMA(lh,n=5)
library(forecast)
#指数平滑
ses(x,alpha=0.3)
ses=(x)
summary(ses(x))
#最优alpha为0.9452

#第2题
plot(airmiles)
x<-airmiles
dat<-ts(x,frequency=4)
holt(airmiles,alpha=0.3,beta=0.1)
holt(dat)
summary(holt(dat))
#最优alpha为0.8085，beta为0.3103

#第三题
plot(AirPassengers)
hw(AirPassengers,alpha=0.1,beta=0.05,gamma=0.3)
hw(AirPassengers)
summary(hw(AirPassengers))
#最佳  alpha = 0.9999 beta  = 1e-04 gamma = 1e-04 

#第4题
forecast(ses(lh),h=4)
forecast(holt(dat),h=4)
forecast(hw(AirPassengers),h=24)
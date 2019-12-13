rm(list = ls())
#模拟garch(1,2)模型
#求ht
eta <- 0.2
lambda <- c(0.3,0.4)
w <- 2
ht <- c(0,2)
set.seed(111)
vt <- rnorm(1000)
for (i in 3:1000) {
  ht[i] <- w + (eta + lambda[1]*vt[i-1]^2)*ht[i-1] + lambda[2]*vt[i-2]^2*ht[i-2]
}
#求et
et <- sqrt(ht)*vt
#确定性信息拟合模型,ARIMA(1,1,1)
phi <- -0.3
theta <- 0.4
xt <- c(0,et[2])
for (i in 3:1000) {
  xt[i] <- (1 + phi)*xt[i-1] + phi*xt[i-2] + et[i] - theta*et[i-1]
}
#检验
x <- ts(xt)
plot(x)  
plot(diff(x))
plot(diff(x)^2)
acf(diff(x))  
pacf(diff(x))  
for (i in 1:2) {
  print(Box.test(diff(x),lag = 6*i))
}
#拟合ARIMA(1,1,1)
x.fit <- arima(x,order = c(1,1,1))
x.fit
for (i in 1:6) {
  print(Box.test(x.fit$residuals,type = 'Lj',lag = i))
}
#条件异方差检验
for (i in 1:6) {
  print(Box.test(x.fit$residuals^2,type = 'Lj',lag = i))
}
library(tseries)
r.fit <- garch(x.fit$residuals,order = c(1,2))
summary(r.fit)

#####################第七次上机作业#################
#习题5.1
rm(list = ls())
xt <- scan('习题数据、案例数据、R代码/习题数据/习题5.1数据.txt')
x <- ts(xt)
plot(x)#不平稳
plot(diff(x))#平稳
#检查数据为非白噪声
for (i in 1:2) {
  print(Box.test(diff(x),lag = 6*i))
}
acf(diff(x))
pacf(diff(x))
library(forecast)
x.fit <- auto.arima(x)
x.fit
#模型诊断，对残差序列进行白噪声检验
for (i in 1:2) {
  print(Box.test(x.fit$residuals,lag = 6*i))
}  #为白噪声
#预测
x.fore <- forecast(x.fit,h=3)
plot(x.fore)

####习题5.2
rm(list = ls())
xt <- read.table('习题数据、案例数据、R代码/习题数据/习题5.2数据.txt',header = T)
xt <- c(xt[,2],xt[,4],xt[,6])
x <- ts(xt,start = 1949)
plot(x)#不平稳
plot(diff(x))#不平稳
plot(diff(diff(x)))#平稳
x.dif <- diff(diff(x))
#检查数据为非白噪声
for (i in 1:2) {
  print(Box.test(x.dif,lag = 6*i))
}
acf(x.dif)#拖尾
pacf(x.dif)#3阶截尾
#模型拟合
x.fit <- arima(x,order = c(1,2,1))
x.fit
#模型诊断，对残差序列进行白噪声检验
for (i in 1:2) {
  print(Box.test(x.fit$residuals,lag = 6*i))
}  #为白噪声
#预测
x61 <- 2.4197*x[60]-1.8394*x[59]+0.4197*x[58]-0.8958*x.fit$residual[60]
x61
x62<-2.4197*x61-1.8394*x[60]+0.4197*x[59]        
x62
x.fore <- forecast(x.fit,h=2)
plot(x.fore)

####习题5.3
rm(list = ls())
xt <- read.table('习题数据、案例数据、R代码/习题数据/习题5.3数据.txt',header = T)
xt <- c(xt[,2],xt[,4],xt[,6])
x <- ts(xt,start = c(1973,1),frequency = 12)
plot(x)#不平稳
x.dif <- diff(diff(x),12)
plot(x.dif)#平稳
acf(x.dif)
pacf(x.dif)
#拟合季节模型
x.fit <- arima(x,order = c(0,1,1),seasonal = list(order=c(0,1,1),period=12))
x.fit
#模型诊断，对残差序列进行白噪声检验
for (i in 1:2) {
  print(Box.test(x.fit$residuals,lag = 6*i))
}  #为白噪声
#预测
x.fore <- forecast(x.fit,h=12)
plot(x.fore)

##################第八次上机#############
#习题5.4
rm(list = ls())
xt <- read.table('习题数据、案例数据、R代码/习题数据/习题5.4数据.txt',header = T)
xt <- c(xt[,2],xt[,4],xt[,6],xt[,8])
x <- ts(xt,start = 1750)
plot(x)#基本平稳
acf(x)
pacf(x)
#拟合ARMA(1,1)模型
x.fit <- arima(x,order = c(1,0,1))
x.fit
#模型诊断，对残差序列进行白噪声检验
for (i in 1:2) {
  print(Box.test(x.fit$residuals,lag = 6*i))
}  #为白噪声
##条件异方差检验
#Portmanteau Q检验
for (i in 1:5) {
  print(Box.test(x.fit$residuals^2,lag = i))
} 
#LM检验
library(FinTS)
for (i in 1:5) {
  print(ArchTest(x.fit$residuals,lag = i))
} 
#拟合garch模型
acf(x.fit$residuals^2)
r.fit <- garch(x.fit$residuals,order = c(0,1))
summary(r.fit)

#习题5.5
rm(list = ls())
xt <- scan('习题数据、案例数据、R代码/习题数据/习题5.5数据.txt')
x <- ts(xt)
plot(x)#不平稳
plot(diff(x))#平稳
#检查数据为非白噪声
for (i in 1:2) {
  print(Box.test(diff(x),lag = 6*i))
}
acf(diff(x))
pacf(diff(x))
#拟合ARIMA(2,1,1)模型
x.fit <- arima(x,order = c(2,1,1))
x.fit
#模型诊断，对残差序列进行白噪声检验
for (i in 1:2) {
  print(Box.test(x.fit$residuals,lag = 6*i))
}  #为白噪声
##条件异方差检验
#Portmanteau Q检验
for (i in 1:5) {
  print(Box.test(x.fit$residuals^2,lag = i))
} 
#LM检验
for (i in 1:5) {
  print(ArchTest(x.fit$residuals,lag = i))
} #方差齐性
#预测
x.fore <- forecast(x.fit,h=6)
x.fore
plot(x.fore)

#习题5.6
rm(list = ls())
xt <- scan('习题数据、案例数据、R代码/习题数据/习题5.6数据.txt')
x <- ts(xt)
plot(x)#不平稳
plot(diff(x))#基本平稳
#检查数据为非白噪声
for (i in 1:2) {
  print(Box.test(x,lag = 6*i))
}
acf(diff(x))
pacf(diff(x))
#拟合ARIMA(1,1,1)模型
x.fit <- arima(x,order = c(1,1,1))
x.fit
#模型诊断，对残差序列进行白噪声检验
for (i in 1:2) {
  print(Box.test(x.fit$residuals,lag = 6*i))
}  #为白噪声
##条件异方差检验
#Portmanteau Q检验
for (i in 1:5) {
  print(Box.test(x.fit$residuals^2,lag = i))
} 
#LM检验
for (i in 1:5) {
  print(ArchTest(x.fit$residuals,lag = i))
} #方差非齐
#拟合garch模型
acf(x.fit$residuals^2)
r.fit <- garch(x.fit$residuals,order = c(0,2))
summary(r.fit)

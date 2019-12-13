library(tseries)
library(FinTS)
library(forecast)
setwd("E:/github_repo/time_series")

################################第七次作业#################################################

#1.GARCH(1,2)数据模拟
##y[t] = mu + c*h[t] + epsilon[t]
##epsilon[t] = e[t]*sqrt(h[t])
##h[t] = omega + beta*h[t-1] + lambda1*epsilon[t-1]^2 + lambda2*epsilon[t-2]^2
set.seed(123)
nT <- 1000;n1 <- 100;T <- n1 +nT
h <- numeric(T)
y <- numeric(T)
epsilon <- numeric(T)
e <- rnorm(T)
mu <- 0
omega <- 0.2
c <- 0.2
lambda1 <- 0.3
lambda2 <- 0.1
beta <- 0.8
h[1] <- (omega/(1-lambda1-lambda2-beta))^2 #指定初始项
epsilon[1] <- e[1]*sqrt(h[1])
y[1] <- mu + c*h[1] + epsilon[1]
for (t in 2:T){
  h[t] <- omega + beta*h[t] + lambda1*epsilon[t]^2 + lambda2*epsilon[t-1]^2
  epsilon[t] <- e[t]*sqrt(h[t])
  y[t] <- mu + c*h[t] + epsilon[t]
}
data <- y[n1+1:T]#剔除前面的时期
ts.plot(data)

#习题5.1
data1 <- scan("习题数据、案例数据、R代码/习题数据/习题5.1数据.txt")
x <- ts(data1)
plot(x)#序列不平稳
x.dif <- diff(x)
plot(x.dif)#一阶差分后数列趋于平稳
for (i in 1:2) print(Box.test(x.dif,lag=i*6))#结果显示差分后序列是白噪声
acf(x.dif) #拖尾
pacf(x.dif) #拖尾 
##拟合ARIMA(1,1,1)
x.fit <- arima(x,order=c(1,1,1))
x.fit
for(i in 1:2) print(Box.test(x.fit$residuals),lag=6*i) #残差序列为白噪声
#预测
x108 <- 1.3395*x[107]-0.3395*x[106]+0.4889*x.fit$residuals[107]
x108
x109 <- 1.3395*x108-0.3395*x[107]
x109
x110 <- 1.3395*x109-0.3395*x108
x110
x.fore<-forecast(x.fit)
x.fore

#习题5.2
data2 <- read.table("习题数据、案例数据、R代码/习题数据/习题5.2数据.txt",head=T)
y <- as.matrix(data2)
y <- c(y[,2],y[,4],y[,6])
y <- ts(y,start = 1949) 
plot(y) #不平稳
y.dif1 <- diff(y)
plot(y.dif1) #一阶差分序列不平稳
y.dif2 <- diff(y.dif1)
plot(y.dif2) #平稳
acf(y.dif2)
pacf(y.dif2)  #拟合ARIMA(0,1,(2,3))模型
for(i in 1:2) print(Box.test(y.dif2,lag=i*6))#二阶差分序列是白噪声
#模型拟合
y.fit <- arima(y,order=c(0,2,3),transform.pars=F,fixed=c(0,NA,NA))#拟合疏系数模型
y.fit
for(i in 1:2) print(Box.test(y.fit$residual,lag=i*6))#残差序列是白噪声
#预测
y61<-2*y[60]-y[59]+0.3393*y.fit$residual[59]+0.2417*y.fit$residual[58]
y61
y62<-2*y61-y[60]+0.3393*y.fit$residual[60]+0.2417*y.fit$residual[59]     
y62
y.fore <- forecast(y.fit)
y.fore


#习题5.3
data3 <- read.table("习题数据、案例数据、R代码/习题数据/习题5.3数据.txt",header=T)
z <- as.matrix(data3)
z <- c(z[,2],z[,4],z[,6])
z <- as.numeric(z)
z <- ts(z,start = c(1973,1),frequency = 12) 
plot(z)#不平稳，且存在周期性
z.dif<-diff(diff(z),12) #作1阶12步差分
plot(z.dif) 
acf(z.dif)#拖尾
pacf(z.dif) #2阶截尾，拟合ARIMA(0,1,2)模型
for(i in 1:2) print(Box.test(z.dif,lag=i*6)) #差分序列是白噪声
# 模型拟合，考虑加入季节因素
z.fit <- arima(z,order=c(0,1,2),seasonal=list(order=c(1,1,1 ),period=12))
z.fit
for (i in 1:2) {print(Box.test(z.fit$residual,lag=i*6))} #残差序列是白噪声
# 预测
z.fore <- forecast(z.fit)
z.fore

#################################第八次作业###############################################
#习题5.4
data4 <- read.table("习题数据、案例数据、R代码/习题数据/习题5.4数据.txt",header=T)
x <- c(data4[,2],data4[,4],data4[,6],data4[,8])
x <- ts(x,start=1750)
plot(x) #平稳
acf(x) #拖尾
pacf(x) #1阶截尾 拟合ARIMA(0,0,1)模型
x.fit <- arima(x,order=c(0,0,1))
x.fit
for (i in 1:2) {print(Box.test(x.fit$residuals,lag=i*6))}#拒绝原假设，残差不是白噪声序列
#LM检验
for (i in 1:5) {print(ArchTest(x.fit$residuals,lag = i))}
#拟合garch(0,1)模型
#拟合garch模型
acf(x.fit$residuals^2)
r.fit <- garch(x.fit$residuals,order = c(0,1))
summary(r.fit)

#习题5.5
y <- scan('习题数据、案例数据、R代码/习题数据/习题5.5数据.txt')
y <- ts(y)
plot(y)#不平稳
y.dif <- diff(y)
plot(y.dif)#平稳
for (i in 1:2) {print(Box.test(y.dif,lag = 6*i))} #检验数据为非白噪声
acf(y.dif) #拖尾
pacf(y.dif) #2截结尾
#拟合ARIMA(2,1,0)模型
z.fit <- arima(x,order = c(2,1,0))
z.fit
#模型诊断，对残差序列进行白噪声检验
for (i in 1:2) {print(Box.test(z.fit$residuals,lag = 6*i))}  #残差序列为白噪声
##条件异方差检验
#LM检验
for (i in 1:5) {print(ArchTest(x.fit$residuals,lag = i))} #方差齐性
#预测
z.fore <- forecast(z.fit,h=6)
z.fore
plot(z.fore)

#习题5.6
#读取数据
z <- scan('习题数据、案例数据、R代码/习题数据/习题5.6数据.txt')
z <- ts(z)
plot(z) #不平稳
z.dif <- diff(z)
plot(z.dif) #差分序列平稳
for (i in 1:2) {print(Box.test(z,lag = 6*i))} #序列非白噪声
acf(z.dif) #拖尾
pacf(z.dif) #拖尾，拟合arima(1,1,1)模型
z.fit <- arima(z,order=c(1,1,1))
z.fit
for (i in 1:2) {print(Box.test(z.fit$residuals,lag = 6*i))} #残差序列为白噪声
#检验方差齐性
res <- z.fit$residuals
plot(res^2) 
#LM检验
for (i in 1:5) {print(ArchTest(z.fit$residuals,lag = i))} #方差非齐
#拟合garch(0,1)
r.fit <- garch(res,order = c(0,1))
summary(r.fit)





###########################################################################################
#自定义求自相关系数的函数auto_acf
auto_acf <- function(ts,k){
  #ts表示输入的时间序列，k表示滞后阶数
  #输出：时间序列ts的k阶自相关系数
  l <- length(ts)
  ts1 <- ts[1:(l-k)]
  ts2 <- ts[(k+1):l]
  ts_mean <- mean(ts)
  ts_var <- sum((ts-ts_mean)**2)
  auto_acf <- 0
  for (i in 1:length(ts1)){
    temp <- (ts1[i]-ts_mean)*(ts2[i]-ts_mean)/ts_var
    auto_acf <- auto_acf + temp
  }
  return(auto_acf)
}


#实验一
dat1 <- read.csv("data/file2.csv", header=T)
plot(dat1$Year, dat1$index, type='l')
#绘制时序图，该时间序列有明显的递增趋势，不是平稳序列
for (i in 1:24) {print(auto_acf(dat1$index,i))}#按公式计算的自相关系数结果
print(acf(dat1$index,lag=24))#与acf函数计算结果差异很小
acf(dat1$index)#绘制样本自相关图
#该序列的自相关系数在很长的延迟时期都没有递减到0，所以该序列是非平稳序列。

#实验二
dat2 <- read.csv("data/file3.csv", header=T)
plot(dat2$year, dat2$sunsplot, type='l')#该序列具有周期性，不是平稳序列
acf(dat2$sunsplot)#根据自相关图进一步判断不是平稳序列
for (i in 1:24) {print(auto_acf(dat2$sunsplot,i))}#计算时间序列的样本自相关系数pk（k=1,2,…，24）
print(acf(dat2$sunsplot,lag=24))#利用acf函数检查
a <- 0
n <- length(dat2$sunsplot)
m <- 6 #滞后阶数为6
for (i in 1:m) {
  b <- auto_acf(dat2$sunsplot,i)**2/(n-i)
  a <- a + b
}
LB <- n*(n+2)*a
p <- pchisq(LB,6,log.p=T)#p值p值远小于α，拒绝原假设，sunsplot序列属于非白噪声序列。
print(Box.test(dat2$sunsplot, lag=6))#自带函数检查，一致


#实验三
dat3 <- read.csv("data/file4.csv", header=T)
plot(dat3$time, dat3$output, type='l')#该序列随时间呈增长趋势，不属于平稳序列
for (i in 1:2) print(Box.test(dat3$output, lag=i*6))
#纯随机检验结果显示，在各阶延迟下LB检验统计量的p值都非常小，可以认为output序列属于非白噪声序列
Xt <- dat3$output
Yt <- c()
for (i in 2:length(Xt)){
  Yt[i] <- Xt[i] - Xt[i-1]
}
Yt <- na.omit(Yt)
Yt <- ts(Yt, start=dat3$time[2])
plot(Yt)
acf(Yt)#根据时序图和自相关图，易知一阶差分序列Yt是平稳序列
for (i in 1:2) print(Box.test(Yt, lag=i*6))
#根据纯随机检验，p值分别为0.8137，0.9922远大于显著性水平α，不能拒绝原假设，
#所以认为Yt是白噪声序列


#######################第三次上机##################################
rm(list = ls())
#模型1:xt=0.5*xt-1 + et

n <- 100
phi1 <- 0.5
x1 <- arima.sim(n=n,list(ar=phi1)) #模拟模型1
ts.plot(x1) #序列平稳
G1 <- c()
G1[1] <- 1
G1[2] <- phi1*G1[1]
for (i in 3:n){
  G1[i]=phi1*G1[i-1]
}
G1[1:10] #格林函数前10项

varx1 <- sum(G1^2*1)
varx1 #方差

gamma <- c()
gamma[1] <- varx1
gamma[2] <- phi1*gamma
for (i in 3:10){
  gamma[i]=phi1*gamma[i-1]
}
gamma #协方差函数前10项

rhox1 <- gamma[1:10]/varx1
rhox1 #自相关系数前10项

##模型2：xt=xt-1 - -0.5*xt-2 + et
phi1 <- 1
phi2 <- -0.5
x2 <- arima.sim(n=100,list(ar=c(phi1,phi2))) #模拟模型2
ts.plot(x2) #该序列平稳

G2 <- c()
G2[1] <- 1
G2[2]<- phi1*G2[1]
for (i in 3:n){
  G2[i]=phi1*G2[i-1]+phi2*G2[i-2]
}
G2[1:10] #格林函数前10项

varx2 <- sum(G2^2*1)
varx2 #方差

gamma <- c()
gamma[1] <- varx2
gamma[2] <- phi1*gamma[1]/(1-phi2)
for (i in 3:10) {
  gamma[i]=phi1*gamma[i-1]+phi2*gamma[i-2]
}
gamma #协方差函数前10项

rhox2 <- gamma[1:10]/varx2
rhox2 #自相关系数前10项

##模型3：xt=-xt-1 - -0.5*xt-2 + et
phi1 <- 0.5
phi2 <- -0.1
x3 <- arima.sim(n=100,list(ar=c(phi1,phi2))) #模拟模型3
ts.plot(x3) #该序列平稳

G3 <- c()
G3[1] <- 1
G3[2] <- phi1*G3[1]
for (i in 3:n){
  G3[i]=phi1*G3[i-1]+phi2*G3[i-2]
}
G3[1:10] #格林函数前10项

varx3 <- sum(G3^2*1)
varx3 #方差

gamma <- c()
gamma[1] <- varx3
gamma[2] <- phi1*gamma[1]/(1-phi2)
for (i in 3:10) {
  gamma[i]=phi1*gamma[i-1]+phi2*gamma[i-2]
}
gamma #协方差函数前10项

rhox3 <- gamma[1:10]/varx3
rhox3 #自相关系数前10


#######################第四次上机##################################
phi <- c(0.1,0.2,-0.3,0.4)
theta <- c(0.6,0.7,0.8)
#ARMA(3,4)转化为AR(100)
I <- c()
I[1] <- 1
I[2] <- theta[1]
I[3] <- theta[1]*I[2]+theta[2]
for (i in 4:100)  {
  I[i]<-sum(theta*I[(i-1):(i-3)])
}
I[2:5] <- I[2:5]-phi
I

###ARMA(3,4)转化为MA(100)
G <- c()
G[1] <- 1
G[2] <- phi[1]
G[3] <- phi[1]*G[2]+phi[2]
G[4] <- phi[1]*G[3]+phi[2]*G[2]+phi[3]
for (i in 5:100)  {
  G[i]<-sum(phi*G[(i-1):(i-4)])
}
G[2:4] <- G[2:4]-theta
G


###写出MA(4)的ACF,根为0.2，0.3,0.6，0.9
library(Ryacas)
yacas("Expand((x-0.2)*(x-0.3)*(x-0.6)*(x-0.9))")
theta2 <- c(-2,1.35,-0.36,0.0324) #由上面表达式可得
ma_acf <- c()
ma_acf[1] <- 1
ma_acf[5] <- theta2[4]/(1+sum(theta2^2))
for( i in 2:4){
  ma_acf[i]=(theta2[i-1]+sum(theta2[1:(5-i)]*theta2[i:4]))/(1+sum(theta2^2))
}
ma_acf
ARMAacf(ma=theta2, lag.max = 4)#检验,结果一致

##写出AR(5)的PACF，根为0.25，0.5，0.6，0.75，0.8
yacas("Expand((x-0.25)*(x-0.5)*(x-0.6)*(x-0.75)*(x-0.8))")
phi2 <- c(2.9,-3.2675,1.77625,-0.46125,0.045)
ar_acf <- ARMAacf(ar=phi2,lag.max = 6)
ar_acf
shift <- function(v,k){
  if (k==0) v else c(tail(v,k),head(v,-k))
}
shift <- Vectorize(shift, "k")
ar_pacf <- function(k){
  for (i in 1:k){
  v <- ar_acf[1:i]
  temp <- shift(v, 0:(i-1)) #自相关系数矩阵
  temp[upper.tri(temp)] <- 0 #化为下三角矩阵
  D <- temp+t(temp) #化为对称矩阵
  diag(D) <- 1  
  Dk = D
  Dk[,i] <-AR.ACF[2:(i+1)] 
  ar_pacf <- det(Dk)/det(D)
  print(ar_pacf)}
}
ar_pacf(5) #AR(5)的PACF
ARMAacf(ar=phi2, lag.max = 5, pacf = T) #检验

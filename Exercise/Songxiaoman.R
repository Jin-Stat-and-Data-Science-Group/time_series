#################第一次上机实验##############
rm(list = ls())
setwd('F:/github_repo/time_series/习题数据、案例数据、R代码/')
#生成向量，并把向量转化为季度时间序列ts
x <- c(1:10)
x <- ts(x,start = c(1999,1),frequency=4)
x
#导入数据，提取子集
GDP <- read.csv("GDP.csv",sep = ',',header = T)
#提取前30年数据，并生成gdpb78.csv文件
gdpb78 <- subset(GDP,Year>=Year[1]&Year<=Year[30])
write.csv(gdpb78,'results/gdpb78.csv',row.names = F)
#去掉前3年的缺失值miss value，生成gdpB.csv文件
gdpB <- na.omit(GDP)
write.csv(gdpB,'results/gdpB.csv',row.names = F)
#取Year、GDP、HR三列，生成gdp4.csv文件
gdp4 <- GDP[,c('Year','GDP','HR')]
write.csv(gdp4,'results/gdp4.csv',row.names = F)
#取1978年后数据，生成gdpa78.csv文件
gdpa78 <- subset(GDP,Year>=1978)
write.csv(gdpa78,'results/gdpa78.csv',row.names = F)

#################第二次上机实验##############
rm(list = ls())
setwd('F:/github_repo/time_series/习题数据、案例数据、R代码/')

###实验一 (书上习题2.2)
#(1)绘制时间序列图，判断时间序列是否平稳
dat1 <- read.table('习题数据/习题2.2数据.txt')
CO2 <- as.vector(t(as.matrix(dat1))) #将数据框转为向量
CO2 <- ts(CO2,start = c(1975,1),frequency = 12)
plot(CO2)
abline(h=334,lty=2)
## 从时序图中可以看出CO2释放量呈递增趋势,有明显的周期性，故序列非平稳。

#(2)计算时间序列的样本自相关系数pk（k=1,2,…，24）
Acf <- function(x,k){
  EX=mean(x)
  DX=var(x)
  n=length(x)
  Acf <- vector()
  for (i in 1:k){
    a=0
    for (t in 1:(n-i)){
      a=a+(x[t]-EX)*(x[t+i]-EX)
    }
    Acf[i]=a/(n-1)/DX 
  }
  return(Acf)
}
Acf(CO2,24) #样本自相关系数
acf(CO2,lag.max = 24)$acf #检验函数正确性

#(3)绘制样本自相关图，并解释该图形。
acf(CO2,lag.max = 24)
##序列自相关系数长期位于零轴的一边，这是具有单调趋势的非平稳序列的一种典型特征，
##同时自相关图呈现出明显的正弦波动规律，这是具有周期变化规律的非平稳序列的典型特征。

###实验二 (书上习题2.3)
dat2 <- read.table('习题数据/习题2.3数据.txt')
rain <-  as.vector(t(as.matrix(dat2)))
rain <- ts(rain,start = c(1945,1),frequency = 12)
#（1）计算时间序列的样本自相关系数pk（k=1,2,…，24）
Acf(rain,24)
acf(rain,lag=24)$acf #检验
#（2）判断时间序列的平稳性；
plot(rain) #时序图
abline(h=mean(rain),lty=2,col="red")
acf(rain) #自相关图
## 由时序图可知，降雨量一直在95mm附近随机波动，没有明显的趋势或周期，可视为平稳序列，通过自相关图进一步判断。
## 序列的自相关系数一直比较小，始终控制在2倍的标准差范围内，可认为该序列一直都在零轴附近波动，可以确定该序列是平稳序列。
#（3）判断时间序列的纯随机性。（阶数为6阶）
Box.test(rain,lag = 6)
##纯随机性检验结果显示，在6阶延迟下p-value = 0.257>0.1,无法拒绝原假设，可以认为该序列是白噪声序列。

###实验二 (书上习题2.6)
dat3 <- read.table('习题数据/习题2.6数据.txt',fill=T)
rob <- as.vector(t(as.matrix(dat3)))
rob <- na.omit(rob)
rob  <- ts(rob ,start = c(1969,1),frequency = 12)
#（1）判断序列{xt}的平稳性及纯随机性；
plot(rob)
abline(h=mean(rob),lty=2,col="red")
acf(rob)
for(i in 1:2)print(Box.test(rob,lag=6*i)) 
## 时序图显示序列在13附近波动，可认为序列平稳。
## 自相关图显示自相关系数在2倍标准差内波动，可认为该序列一直都在零轴附近波动，认为序列是平稳序列
## 随机性检验结果显示，在各阶延迟下，P值很小（<0.05),可以认为每28天内的抢包案件数序列属于非白噪声序列。

#（2）对序列进行函数运算：yt=xt-xt-1，并判断序列{yt}的平稳性及纯随机性。
yrob <- diff(rob)
plot(yrob)
abline(h=0,lty=2,col="red")
acf(yrob)
for(i in 1:2)print(Box.test(yrob,lag=6*i)) 
## 时序图显示序列在0附近波动，可认为序列平稳。
## 自相关图显示自相关系数在2倍标准差内波动，可认为该序列一直都在零轴附近波动，认为序列是平稳序列
## 随机性检验结果显示，在各阶延迟下，P值很小（<0.05),可以认为每28天内的抢包案件数序列属于非白噪声序列。

#################第三次上机实验##################
rm(list = ls())
#使用airma.sim函数模拟时间序列数据，并作图,格林函数的序列（前10项）,方差,自协方差函数（前10项）,自相关系数（前10项）
##AR(1):xt=0.8xt-1
set.seed(111)
n=100
sigma=1
phi1=0.8
et <- rnorm(n, 0, sigma)
xt <- vector(length = 100)
xt[1] <- et[1]
for (i in 2:n){
  xt[i] <- phi1*xt[i-1]+et[i]
}
xt
plot(xt, type = "l")
##该序列平稳
##airma.sim函数拟合
set.seed(111)
ts1 <- arima.sim(n=100, list(ar=0.8), sd=1)
ts.plot(ts1)
#格林函数序列
G <- vector()
G0=1
phi1=0.8
G1=phi1*G0
G2=phi1*G1
G[1:2]=c(G0,G1)
for (i in 3:10) {
  G[i]=phi1*G[i-1]
}
G
#方差
varxt <- sum(G^2*(sigma^2))
Varxt
#自协方差函数（前10项）
gamma =vector()
gamma0=varxt
gamma1=phi1*gamma0
gamma[1:2]=c(gamma0,gamma1)
for (i in 3:10) {
  gamma[i]=phi1*gamma[i-1]
}
gamma
#自相关系数（前10项）
rho=vector()
rho0=1
rho1=phi1*rho0
rho[1:2]=c(rho0,rho1)
for (i in 3:11) {
  rho[i]=phi1*rho[i-1]
}
rho
ARMAacf(ar=phi1, lag.max = 10) #检验

##AR(2):xt=-0.1xt-1+0.5xt-2+et
set.seed(123)
n=100
sigma<- 2
phi1 <- -0.1
phi2 <- 0.5
et <- rnorm(n, 0, sigma)
xt <- vector(length = 100)
xt[1] <-et[1] 
xt[2] <-et[2] 
for (i in 3:n){
  xt[i] <- phi1*xt[i-1]+phi2*xt[i-2]+et[i]
}
xt
ts.plot(xt)
##airma.sim函数拟合
set.seed(123)
ts2 <- arima.sim(n=100, list(ar=c(-0.1,0.5)), sd=2)
ts.plot(ts2)
## 由时序图可知，该序列平稳
#格林函数序列
G <- vector()
G0=1
phi1=-0.1
phi2=0.5
G1=phi1*G0
G2=phi1*G1+phi2*G0
G[1:2]=c(G0,G1)
for (i in 3:10) {
  G[i]=phi1*G[i-1]+phi2*G[i-2]
}
G
#方差
Varxt =sum(G^2*(sigma^2))
Varxt
#自协方差函数（前10项）
gamma =vector()
gamma0=Varxt
gamma1=phi1*gamma0/(1-phi2)
gamma[1:2]=c(gamma0,gamma1)
for (i in 3:10) {
  gamma[i]=phi1*gamma[i-1]+phi2*gamma[i-2]
}
gamma
#自相关系数（前10项）
rho=vector()
rho0=1
rho1=phi1*rho0/(1-phi2)
rho[1:2]=c(rho0,rho1)
for (i in 3:11) {
  rho[i]=phi1*rho[i-1]+phi2*rho[i-2]
}
rho
ARMAacf(ar=c(phi1,phi2), lag.max = 10) #检验

##AR(3)模型：平稳要求：特征根在单位圆内，则设特征方程(x-0.5)(x+0.4)(x-0.2)=0
library(Ryacas) 
expr <- yacas("Expand((x-0.5)*(x+0.4)*(x-0.2))")
expr
##AR(3)模型:xt=0.3x(t-1)+0.18x(t-2)-0.04x(t-3)
set.seed(123)
ts3 <- arima.sim(n=100, list(ar=c(0.3,0.18,-0.04)), sd=1)
ts.plot(ts3)
#格林函数序列
G <- vector()
G0=1
phi1=0.3
phi2=0.18
phi3=-0.04
G1=phi1*G0
G2=phi1*G1+phi2*G0
G3=phi1*G2+phi2*G1+phi3*G0
G[1:3]=c(G0,G1,G2)
for (i in 4:10) {
  G[i]=phi1*G[i-1]+phi2*G[i-2]+phi3*G[i-3]
}
G
#方差
Varxt =sum(G^2*(1^2))
Varxt
#自协方差函数（前10项）
gamma =vector()
gamma0=Varxt
gamma1=(phi1+phi2*phi3)*gamma0/(1-phi2-phi1*phi3-phi1*phi1)
gamma2=phi2*gamma0+(phi1+phi3)*gamma1
gamma3=phi1*gamma2+phi2*gamma1+phi3*gamma0
gamma[1:3]=c(gamma0,gamma1,gamma2)
for (i in 4:10) {
  gamma[i]=phi1*gamma[i-1]+phi2*gamma[i-2]+phi3*gamma[i-3]
}
gamma
#自相关系数（前10项）
rho=gamma/gamma0
rho  #近似相关系数
ARMAacf(ar=c(0.3,0.18,-0.04), lag.max = 10) #检验

#################第四次上机实验##################
#1.ARMA(4，3)
#AR的根为0.1,0.2,-0.3,0.4
#MA的根为0.6,0.7,0.8
#把序列转化为AR(∞) 求前100项系数 
phi <- c(0.1,0.2,-0.3,0.4)
theta <- c(0.6,0.7,0.8)
I <- vector(length = 100)
I0 <- 1
I1 <- 0.6
I2 <- 0.6*I1+0.7*I0
I[1:3] <- c(I0, I1, I2)
for (j in 4:100){
  I[j] <- sum(theta*I[(j-1):(j-3)])
}
AR.c <- I
AR.c[2:5] <- I[2:5]-phi
AR.c
#把序列转化为MA(∞) 求前100项
G0=1
G1=0.1
G2=0.1*G1+0.2*G0
G3=0.1*G2+0.2*G1-0.3*G0
G[1:4]=C(G0,G1,G2,G3)
for (i in 5:100){
  G[i] <- sum(phi*G[(i-1):(i-4)])
}
MA.c <- G
MA.c[2:4] <- G[2:4]-theta
MA.c

#2.写出MA(4)的ACF
#根为0.2,0.3,0.6,0.9
expr1 <- yacas("Expand((x-0.2)*(x-0.3)*(x-0.6)*(x-0.9))")
expr1 
theta <- c(1.1,0.1,-0.392,0.096)
rho <- vector()
rho0 <- 1
rho4 <- theta[4]/(1+sum(theta^2))
rho[c(1,5)] <- c(rho0,rho4)
for( k in 2:4){
  rho[k]=(theta[k-1]+sum(theta[1:(5-k)]*theta[k:4]))/(1+sum(theta^2))
}
MA.ACF <- rho
MA.ACF
ARMAacf(ma=theta, lag.max = 4)#检验

#3.写出AR(5)的PACF
#根为0.25，0.5，0.6，0.75，0.8
expr2 <- yacas("Expand((x-0.25)*(x-0.5)*(x-0.6)*(x-0.75)*(x-0.8))")
expr2 #求AR(5)的表达式
phi <- c(2.9,-3.2675,1.77625,-0.46125,0.045)#AR(5)的系数
rho <- ARMAacf(ar=phi,lag.max = 6) #AR(5)的6个自相关系数
rho
shift <- function(v,k){
  if (k==0) v else c(tail(v,k),head(v,-k))
} #矩阵转换
shift <- Vectorize(shift, "k")#将一个不能进行向量化运算的函数进行转化，使之具备向量化运算功能。
AR.PACF <- function(k){
  v <- rho[1:k]
  temp <- shift(v, 0:(k-1)) #自相关系数矩阵
  temp[upper.tri(temp)] <- 0 #化为下三角矩阵
  D <- temp+t(temp)
  diag(D) <- 1  #化为对称矩阵
  Dk = D
  Dk[,k] <-rho[2:(k+1)] 
  AR.PACF <- det(Dk)/det(D)
  print(AR.PACF)
}
for (i in 1:5) AR.PACF(i) #AR(5)的PACF
ARMAacf(ar=phi, lag.max = 5, pacf = T) #检验

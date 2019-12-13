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
##AR(1):xt=0.8xt-1+et
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
varxt 
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

# MA(2)的数据模拟
#xt=et-0.3et-1-0.5et-2
set.seed(123)
n=100
sigma=1
et <- rnorm(n, 0, sigma)
et
xt <- vector()
et0=0
etfu1=0
xt[1]=et[1]-0.3*et0-0.5*etfu1
xt[2]=et[2]-0.3*et[1]-0.5*et0
xt[3]=et[3]-0.3*et[2]-0.5*et[1]
for (i in 3:100) {
  xt[i]=et[i]-0.3*et[i-1]-0.5*et[i-2]
}
xt
plot(xt,type = "l")
arima(xt,order=c(0,0,2),include.mean = F)


#################第四次上机实验##################
library(Ryacas) 
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
G[1:4]=c(G0,G1,G2,G3)
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
##pcaf函数
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
##另一种方法
ar.pacf <- function(k){
  D = matrix(rep(0,k^2),k,k)
  for (i in 1:k) {
    D[i,(i:k)]=rho[1:(k-i+1)] #上三角
    }
  D=t(D)+D #对称阵
  diag(D)=1  #对角线=1
  Dk=D
  Dk[,k]=rho[2:(k+1)]
  phikk=det(Dk)/det(D)
  ar.pacf=phikk
  print(ar.pacf)
}
for (i in 1:5) ar.pacf(i)

#################第五次上机实验##################
rm(list=ls())
setwd("F:/github_repo/time_series/习题数据、案例数据、R代码")
#运用附录1.9的数据

#1、拟合模型ma（1），arma（1,1）
library(xlsx)
dat<- read.xlsx('案例数据/附录1.9.xlsx',1)
overshort <- ts(dat$overshort)
plot(overshort)#时序图显示为平稳序列
##白噪声检验
for (i in 1:2)  print(Box.test(overshort,type = 'Lj',lag = 6*i))
### 拒绝原假设，为非白噪声序列
acf(overshort)
### 延迟1阶的自相关系数在2倍标准差之外，其他阶数均在2倍标准差内，具有短期相关性，1阶截尾
pacf(overshort)
### 偏相关系数轨迹呈正弦波动，具有拖尾特征
### 综上初步确定拟合模型为MA(1)模型
## ma(1)参数估计
overshort.fit1 <- arima(overshort,order=c(0,0,1),method = "CSS-ML")
overshort.fit1 
coef(overshort.fit1)
### ma(1)为：xt=-4.7945338 +et-0.8476717et-1
## ma(1)拟合模型残差检验
for (i in 1:2)  print(Box.test(overshort.fit1$residuals,type = 'Lj',lag = 6*i))
### 不能拒绝原假设残差模型可视为白噪声序列，说明ma(1)显著有效
## 参数显著性检验
t1.ma1=-0.8477/0.1206
t1.ma1
pt(t1.ma1,df=55,lower.tail = T)
t1.machang=-4.7945/ 1.0252
t1.machang
pt(t1.machang,df=55,lower.tail = T)
### 检验结果显示，系数均显著非零
## 拟合arma(1,1)
overshort.fit2 <- arima(overshort,order=c(1,0,1),include.mean = F,method = "CSS-ML")
overshort.fit2
### arma(1,1)为：xt=-0.0742xt-1+et-0.6169et-1
## arma(1,1)拟合模型残差检验
for (i in 1:2)  print(Box.test(overshort.fit2$residuals,type = 'Lj',lag = 6*i))
### 不能拒绝原假设残差模型可视为白噪声序列，说明arma(1,1)显著有效
## 参数显著性检验
t2.ar1=-0.0742/ 0.1805 
t2.ar1
pt(t2.ar1,df=55,lower.tail = T)
t2.ma1=-0.6169/0.1291 
t2.ma1
pt(t2.ma1,df=55,lower.tail = T)
### 检验结果显示，ar1系数不显著，ma1系数显著系数均显著非零

#2、根据公式计算两个模型的AIC和BIC，据此判断最优模型
##ma(1)
AIC1 <- -2*overshort.fit1$loglik+2*3
BIC1 <- -2*overshort.fit1$loglik+log(57)*3
##arma(1,1)
AIC2 <- -2*overshort.fit2$loglik+2*3
BIC2 <- -2*overshort.fit2$loglik+log(57)*3
AIC1-AIC2
BIC1-BIC2
### MA(1)模型的AIC和BIC均比ARMA(1,1)的要小，因此选择MA(1)

#3、计算arma（1,1）的预测值，并计算58,59,60期的预测方差
##arma（1,1）的预测值
et <- resid(overshort.fit2)
overshort.58hat=-0.0742*overshort[57]-0.6169*e[57]
overshort.59hat=-0.0742*overshort.58hat
overshort.60hat=-0.0742*overshort.59hat
predict(overshort.fit2, n.ahead = 3) #检验
##58,59,60期的预测方差
G0=1
G1=-0.0742*G0-0.6169
G2=-0.0742*G1
G=vector()
G[1:3]=c(G0,G1,G2)
var.overshort.58hat=(G0^2)*overshort.fit2$sigma2
var.overshort.59hat=sum(G[1:2]^2)*overshort.fit2$sigma2
var.overshort.60hat=sum(G[1:3]^2)*overshort.fit2$sigma2

#################第七次上机实验##################
#根据5.1，5.2，5.3数据
#1．	将数据转化为序列
#2．	画时序图观测特征，判断序列是否平稳
#3．	做差分，画差分时序图，看哪个效果比较好
#4．	检验数据是不是白噪声
#5．	对差分数据进行识别
#6．	建立arima模型，模型诊断，对残差序列进行白噪声检验
#7．	做预测，5.1和5.2计算点预测
#8．	用forecast函数进行预测，并画图
rm(list=ls())
setwd('F:/github_repo/time_series/习题数据、案例数据、R代码/习题数据')
library(forecast)
#5.1数据
dat1 <- read.table('习题5.1数据.txt',fill=T)
x <- as.vector(t(as.matrix(dat1)))
x <- na.omit(x)
x <- ts(x) #转为时间序列
ts.plot(x)
##从时序图可以看出，序列并不平稳
x.dif1 <-diff(x)
plot(x.dif1)#1阶差分
##从差分时序图可以看出1阶差分后序列趋于平稳
for (i in 1:2)  print(Box.test(x.dif1,lag = 6*i)) #白噪声检验
##不拒绝原假设，说明1阶差分序列是白噪声序列
acf(x.dif1)#自相关阶数均在2倍标准差范围内，说明原序列1阶截尾  
pacf(x.dif1)#偏自相关系数具有拖尾性
##1阶差分后，自相关系数1阶截尾，偏自相关系数具有拖尾性，初步确定为ARIMA(0,1,1)
x.fit <- arima(x,order=c(0,1,1))#模型拟合
x.fit
##拟合模型为：xt=xt-1+sigmat-0.1549*sigmat-1
for (i in 1:2)  print(Box.test(x.fit$residuals,lag = 6*i)) #残差白噪声检验
##残差白噪声检验没有拒绝原假设，说明该模型显著成立
x108hat=x[107]-0.1549*x.fit$residuals[107]
x108hat
x109hat=x108hat  #点预测
x.fore<-forecast(x.fit,h=10)
x.fore #函数预测
plot(x.fore)

#5.2数据
dat2 <- read.table('习题5.2数据.txt',header = T)
y <- as.matrix(dat2)
y <- c(y[,2],y[,4],y[,6])
y <- ts(y,start = 1949) 
plot(y)
## 从时序图中可以看出序列具有递增趋势，非平稳
y.dif1 <- diff(y)
plot(y.dif1)
y.dif2 <- diff(y,1,2)
plot(y.dif2)  #基本平稳
for (i in 1:2)  print(Box.test(y.dif2,lag =6*i)) #白噪声检验
##不拒绝原假设，说明2阶差分序列是白噪声序列
acf(y.dif2)
pacf(y.dif2)
## 2阶差分后，自相关系数1阶截尾，偏自相关系数拖尾，初步确定模型为ARIMA(0,2,1)
y.fit <- arima(y,order=c(0,2,1))#模型拟合
y.fit
for (i in 1:2)  print(Box.test(y.fit$residuals,lag = 6*i)) #残差白噪声检验
##残差白噪声检验滞后6阶拒绝原假设，说明该模型不成立，拟合ARIMA(1,2,1)
y.fit <- arima(y,order=c(1,2,1))#模型拟合
y.fit
for (i in 1:2)  print(Box.test(y.fit$residuals,lag = 6*i)) #残差白噪声检验
##残差白噪声检验没有拒绝原假设，说明该模型显著成立
##拟合模型为：(1+0.4197B)(1-B)^2*xt=sigmat-0.8958*sigmat-1
##即xt=2.4197*xt-1-1.8394*xt-2+0.4197*xt-3+sigmat-0.8958*sigmat-1
y61hat=2.4197*y[60]-1.8394*y[59]+0.4197*y[58]-0.8958*y.fit$residuals[60]
y61hat
y62hat=2.4197*y61hat-1.8394*y[60]+0.4197*y[59]
y62hat
y63hat=2.4197*y62hat-1.8394*y61hat+0.4197*y[60]
y63hat
y64hat=2.4197*y63hat-1.8394*y62hat+0.4197*y61hat
y64hat  #点预测
y.fore<-forecast(y.fit,h=10)
y.fore #函数预测
plot(y.fore)

#5.3数据
dat3 <- read.table('习题5.3数据.txt',header = T)
z <- as.matrix(dat3)
z <- c(z[,2],z[,4],z[,6])
z <- as.numeric(z)
z <- ts(z,start = c(1973,1),frequency = 12) 
plot(z)
## 从时序图中可以看出序列季节性波动
z.dif<-diff(diff(z),12) #1阶12步差分
plot(z.dif)
acf(z.dif)
pacf(z.dif)
##自相关系数显示12阶以内1阶截尾，偏自相关系数12阶以内1阶截尾，尝试用ARMA(1,1)提取差分序列的短期自相关信息
##自相关图显示延迟12阶自相关系数显著非零，而偏自相关图显示延迟12阶偏自相关系数显著非零，这时用以12步为周期的ARMA(1,1)12模型提取差分后序列的季节自相关信息
z.fit <- arima(z,order = c(1,1,1),seasonal = list(order=c(1,1,1 ),period=12))
z.fit
for (i in 1:6)  print(Box.test(z.fit$residuals,lag = 6*i)) #残差白噪声检验
##不拒绝原假设，说明该模型显著成立
z.fore<-forecast(z.fit)
z.fore #函数预测
plot(z.fore)

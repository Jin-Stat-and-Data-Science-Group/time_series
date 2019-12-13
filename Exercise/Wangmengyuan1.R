###############时间序列分析第一次上机实验内容#######################
rm(list = ls())
setwd("D:/github_repo/time_series/习题数据、案例数据、R代码")
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


###############时间序列分析第二次上机实验内容################################
#实验一
rm(list = ls())
setwd('D:/时间序列分析/习题数据、案例数据、R代码/')
dat1 <- read.csv("data/file1.csv",header = T)
#变量yield的时间序列图
plot(dat1$yield~dat1$Year,xlab='Year',ylab = 'yield',type='o',col='blue')
#样本自相关系数
Acf <- function(x,k){
  n=length(x)
  fenzi=0
  xba <- mean(x)
  fenmu <- (n-1)*var(x)
  for (i in 1:(n-k)) {
    s <- (x[i]-xba)*(x[i+k]-xba)
    fenzi <- fenzi+s
  }
  Corr <- fenzi/fenmu
  return(Corr)
}
for (i in 1:24) print(Acf(dat1$yield,i))#输出样本自相关系数
corr <- acf(dat1$yield,lag.max = 24)
corr$acf#检验和上面函数结果一致
#自相关图显示该序列的自相关系数一直比较小，始终控制在2倍标准差范围内，可以认为该序列、
#自始至终都在零轴附近随机波动，不存在趋势和周期性，可以认为该序列是平稳的。

#实验二
Index <- read.csv('data/file2.csv',header = T)
Index
plot(Index$index~Index$Year,type='o',lwd=2,col="blue",main="Index时序图")#画出时间序列图
#绘制样本自相关图，计算样本自相关系数
for (i in 1:24) {
  print(Acf(Index$index,i))
}
corr2 <- acf(Index$index,lag.max = 24)
corr2
#时序图有明显的递增趋势，样本自相关系数始终在2倍标准差范围内，该序列是非平稳时间序列

#判断时间序列的纯随机性
#构造检验统计量——Q统计量（n=19，样本容量小）
n=19#序列观测期数
m=6#延迟期数
Q=n*sum((corr2$acf^2)[1:m])
p=1-pchisq(Q,m,ncp=0,lower.tail = T,log.p = F)
p
Box.test(Index$index,lag = 6,type = "Box-Pierce")
#P值为1.110223e-16，在显著性水平为0.05下，拒绝原假设，该序列不是白噪声序列，不具有纯随机性


#实验三
x <- read.csv('data/file3.csv')
plot(x$sunsplot,type="o",col="green",lwd=2)
#时序图有上升趋势，明显不是平稳的
for (i in 1:24) print(Acf(x$sunsplot,i))
corr3 <- acf(x$sunsplot,lag.max = 24)
corr3$acf
Box.test(x$sunsplot,lag = 6,type = "Box-Pierce")
#拒绝原假设，不具有纯随机性，不是白噪声序列

#在大样本条件下，计算LB统计量来判断纯随机性
n <- length(x$sunsplot)
m=6
LB=0
for (i in 1:m) {
  s=n*(n+2)*(((corr3$acf^2)[i])/(n-i))
  LB=LB+s
}
LB
P <- 1-pchisq(LB,m,ncp=0,lower.tail = T,log.p = F)
P
#P值为0，拒绝原假设，因此该序列不具有纯随机性
y <- c()
for (i in 1:n) {
  y[i] <- x$sunsplot[i]-x$sunsplot[i-1]
}
y
y <- na.omit(y)#去NA
n1 <- length(y)
plot(y,type = "o",col="blue",lwd=1.8,main = "y-时序图",xlab="year")
for (i in 1:24) {
  print(Acf(y,i))
}
corr4 <- acf(y,lag.max = 24)
corr4$acf
#根据自相关图，时间序列y是非平稳的
#要是时间序列平稳，可进行二次差分
LB=0
for (i in 1:m) {
  s=n1*(n1+2)*(((corr4$acf^2)[i])/(n1-i))
  LB=LB+s
}
LB
P <- 1-pchisq(LB,m,ncp=0,lower.tail = T,log.p = F)
P
Box.test(y,lag = 6,type = 'Box-Pierce')#拒绝原假设
#P值小于0.05，拒绝原假设，不具有纯随机性
z <- c()
for (i in 1:n) {
  z[i] <- y[i]-y[i-1]
}
z
z <- na.omit(z)#去NA
n2 <- length(z)
plot(z,type = "o",col="blue",lwd=1.8,main = "y-时序图",xlab="year")
corr5 <- acf(z,lag.max = 24)
#95%的自相关系数都控制在2倍标准差之内，故序列具有平稳性
corr5$acf
Box.test(z,lag = 6,type = "Box-Pierce")
#P值为0.116，大于显著性水平0.05，没有充分的理由拒绝原假设，
#因此，二次差分后的序列是白噪声序列，具有纯随机性和平稳性



######################时间序列分析第三次上机实验内容################################
#1.使用arima.sim函数模拟时间序列数据，并作图
#2.判断时间序列的平稳性
#3.分别得到3个模型的格林函数的序列（前10项）
#4.得到3个模型的方差
#5.计算自协方差函数（前10项）
#6.计算自相关系数（前10项）


################################  AR模型  ##########################
#xt=xt-1-0.5xt-2+et,AR(2)格林函数

set.seed(123)
n=100
x <- c()
et <- rnorm(n,0,1)
for (i in 3:100) {
  x[1]=et[1]
  x[2]=et[2]
  x[i]=x[i-1]-0.5*x[i-2]+et[i]
}
plot(x,type = 'o')
x1 <- arima.sim(n=100,list(ar=c(1,-0.5),order(method = "auto")),sd=1)#用arima.sim函数检验
ts.plot(x1,type='o')#可以直观判断时间序列平稳
G <- c()
ss <- c()
G[1]=1
phi=c(1,-0.5)
p=2
for (j in 2:11){ 
  for (k in 1:(j-1)) {
    if(k<=p) phi[k]=phi[k] else phi[k]=0
    ss[k] <- phi[k]*G[j-k]
  }
  G[j] <- sum(ss)
}
G[2:11]#格林函数前10项
#假设随机干扰序列的均值为0，方差为1
var1 <- (1-phi[2])/((1+phi[2])*(1-phi[1]-phi[2])*(1+phi[1]-phi[2]))#模型方差
gamma0=var1
gamma1=(phi[1]/(1-phi[2]))*gamma0
gamma <- c()
gamma[1] <- gamma0
gamma[2] <- gamma1
for (i in 3:11) {
  gamma[i]=phi[1]*gamma[i-1]+phi[2]*gamma[i-2]
}
gamma[2:11]#自协方差函数前10项
rho1 <- gamma[2:11]/gamma0#自相关系数前11项
ARMAacf(ar=c(1,-0.5),lag.max = 10)
#用ARMAacf函数对对计算得到的自相关系数rho1进行检验，结果相同

################################ MA模型 ##################################
x2 <- arima.sim(n=1000,list(ma=-2))
acf(x2)#MA(2)模型逆函数
I <- c()
tt <- c()
I[1]=1
theta=c(4/5,-16/25)
q=2
for (j in 2:11){ 
  for (k in 1:(j-1)) {
    if(k<=q) theta[k]=theta[k] else theta[k]=0
    tt[k] <- theta[k]*I[j-k]
  }
  I[j] <- sum(tt)
}
I
#假设随机干扰序列的均值为0，方差为1,求自协方差函数（前10项）
var2 <- 1+sum(theta^2)#方差
var2
GAMMA <- c()
mid <- c()
for (k in 1:q) {
  for (i in 1:(q-k)) {
    mid[i] <- theta[i]*theta[k+i]
  }
  GAMMA[k] <- -theta[k]+sum(mid)
}
GAMMA[3:10] <- 0
GAMMA#自协方差函数（前10项）
rho2 <- GAMMA/var2#自相关系数（前10项）
ARMAacf(ma=theta,lag.max = 10)#检验结果不一样
#############   ARMA(1,1)模型   #####################
rm(list = ls())
x3 <- arima.sim(n=1000,list(ar=0.5,ma=-0.8))
acf(x3)
pacf(x3)
phi <- 0.5
theta <- 0.8
G <- c()
I <- c()
G[1]=1
I[1]=1
q=1
p=1
vv <- c()
for (j in 2:11) {
  for (k in 1:(j-1)) {
    if(k<=p) phi[k]=phi[k] else phi[k]=0
    vv[k] <- phi[k]*G[j-k]
  }
  G[j] <- sum(vv)
}
G[2] <- G[2]-theta
G[2:11]#ARMA模型格林函数前10项
for (j in 2:10000) {
    for (k in 1:(j-1)) {
         if(k<=p) phi[k]=phi[k] else phi[k]=0
         if(k<=q) theta[k]=theta[k] else theta[k]=0
         vv[k] <- phi[k]*G[j-k]
       }
     G[j] <- sum(vv)-theta[k]
}
G 
#假设随机干扰序列的均值为0，方差为1
var3 <- sum(G[1:10000]^2)#ARMA模型方差
var3
GAmma <- c()
Mid <- c()
for (k in 1:10) {
  for (i in 1:9990) {
    Mid[i] <- G[i]*G[i+k]
  }
  GAmma[k] <- sum(Mid)
}
GAmma#自协方差函数的前10项
rho3 <- GAmma/var3
rho3#自相关系数的前10项

######################时间序列分析第四次上机实验内容###########################
#1.ARMA(4，3)
#AR的根为0.1,0.2,-0.3,0.4
#MA的根为0.6,0.7,0.8
#把序列转化为AR(∞) 求前100项
#把序列转化为MA(∞) 求前100项
rm(list = ls())
Phi <- c(0.1,0.2,-0.3,0.4)
Theta <- c(0.6,0.7,0.8)
#传递形式
Green <- c()
Green[1]=1
p=4
VV <- c()
for (j in 2:100) {
  for (k in 1:(j-1)) {
    if(k<=p) Phi[k]=Phi[k] else Phi[k]=0
    VV[k] <- Phi[k]*Green[j-k]
  }
  Green[j] <- sum(VV)
}
Green[2:4] <- Green[2:4]-Theta[1:3]
Green
#逆转形式
q=3
I <- c()
I[1]=1
TT <- c()
for (j in 2:100){ 
  for (k in 1:(j-1)) {
    if(k<=q) Theta[k]=Theta[k] else Theta[k]=0
    TT[k] <- Theta[k]*I[j-k]
  }
  I[j] <- sum(TT)
}
I[2:5] <- I[2:5]-Phi[1:4]
I
#2.写出MA(4)的ACF
#根为0.2，0.3,0.6，0.9
rm(list = ls())
library(Ryacas)
yacas("Expand((x-0.2)*(x-0.3)*(x-0.6)*(x-0.9))")
theta <- c(-2,1.35,-0.36,0.0324)
ma_rho <- c()
ma_rho[1] <- 1
ma_rho[5] <- -theta[4]/(1+sum(theta^2))
for (k in 2:4) {
  ma_rho[k] <- (-theta[k]+sum(theta[1:(5-k)]*theta[k:4]))/(1+sum(theta^2))
}
ma_rho[6:10] <- 0
ma_rho#自协方差函数（前10项）
ARMAacf(ma=c(-2,1.35,-0.36,0.0324),lag.max = 10)

#3.写出AR(5)的PACF
#根为0.25，0.5，0.6，0.75，0.8
rm(list = ls())
rho <- ARMAacf(ar=c(0.25,0.5,0.6,0.75,0.8),lag.max = 10)
rho
ar_pacf <- function(k){
  D <- matrix(0,k,k)
    for (i in 1:k) {
      D[i,(i:k)] <- rho[1:(k-i+1)]
    }
    D=t(D)+D
    diag(D) <- 1
    DK <- D
    DK[,k] <- rho[2:(k+1)]
    phi_kk <- det(DK)/det(D)
    print(phi_kk)
  
}
for (k in 1:6) ar_pacf(k)

###########时间序列分析第五次上机实验内容###############
#1、拟合模型
#ma（1），arma（1,1）
#2、根据公式计算两个模型的AIC和BIC，据此判断最优模型
#3、计算arma（1,1）的预测值，并计算58,59,60期的预测方差
#ma(1)模型
rm(list = ls())
library(xlsx)
x <- read.xlsx("D:/github_repo/time_series/习题数据、案例数据、R代码/案例数据/附录1.9.xlsx",1)
x <- ts(x[,2])
plot(x)
for (i in 1:2) {
  print(Box.test(x,type = "Ljung-Box",lag = 6*i))
}
acf(x)#1阶截尾
pacf(x)#偏自相关系数拖尾
#序列拟合MA（1）模型
x.fit1 <- arima(x,order = c(0,0,1))#拟合1阶MA模型
x.fit1
install.packages('zoo')
install.packages('forecast')
library(zoo)
library(forecast)
for (i in 1:2) print(Box.test(x.fit1$residuals,lag = 6*i))#模型的显著性检验
#6阶和12阶延迟下LB统计量的P值都大于显著性水平0.05，可以认为这个拟合模型的残差序列属于白噪声序列，拟合模型显著有效
#参数的显著性检验
n <- length(x)
m <- 2
ma1_t1 <- -0.8477/0.1206
ma1_pt1 <- pt(ma1_t1,(n-m),lower.tail = T)#ma1系数的显著性检验
ma1_pt1
ma1_t2 <- -4.7945/1.0252
ma1_pt2 <- pt(ma1_t2,(n-m),lower.tail = T)#常数的显著性检验
ma1_pt2
#经检验两个系数都显著非零
ma1_AIC <- -2*x.fit1$loglik+2*(0+1+2)
ma1_BIC <- -2*x.fit1$loglik+log(n)

#arma（1,1）模型
y.fit <- arima(x,order = c(1,0,1),include.mean = F)
for (i in 1:2) print(Box.test(y.fit$residuals,lag = 6*i))#模型的显著性检验
#6阶和12阶延迟下LB统计量的P值都大于显著性水平0.05，可以认为这个拟合模型的残差序列属于白噪声序列，拟合模型显著有效
#参数的显著性检验
m2 <- 3
ma1_t1 <- 0.1231/0.1332
ma1_pt1 <- pt(ma1_t1,(n-m2),lower.tail = F)#ar1系数的显著性检验
ma1_pt1
ma1_t2 <- -1/0.065
ma1_pt2 <- pt(ma1_t2,(n-m2),lower.tail = T)#ma1系数的显著性检验
ma1_pt2
ma1_t3 <- -5.0268/0.3879
ma1_pt3 <- pt(ma1_t2,(n-m2),lower.tail = F)#常数的显著性检验
ma1_pt3
arma11_AIC <- -2*y.fit$loglik+2*(1+1+2)
arma11_BIC <- -2*y.fit$loglik+log(n)
ma1_AIC - arma11_AIC
ma1_BIC - arma11_BIC
x.fore <- forecast(y.fit,h=3)#预测未来3期
x.fore
G0 <- 1
G1 <- 0.1231*G0-1
G2 <- 0.1231*G1
var1_hat <- G0^2*y.fit$sigma2
var2_hat <- (G0^2+G1^2)*y.fit$sigma2
var3_hat <- (G0^2+G1^2+G2^2)*y.fit$sigma2
########################时间序列分析第五次上机实验内容###############
##############5.1##############
rm(list = ls())
dat1 <- scan("D:/github_repo/time_series/习题数据、案例数据、R代码/习题数据/习题5.1数据.txt")
dat1 <- ts(dat1)
plot(dat1)#根据时序图来看，序列明显是非平稳的
diff_dat1 <- diff(dat1,1,2)#差分后平稳
plot(diff_dat1)
acf(diff_dat1)#acf一阶截尾
pacf(diff_dat1)#pacf拖尾，因此可以对差分后的序列拟合ma(1)模型
for (i in 1:2) print(Box.test(diff_dat1,lag = 6*i))#白噪声检验
install.packages("zoo")
install.packages("forecast")
library(zoo)
library(forecast)
dat1.fit <- auto.arima(diff_dat1)
dat1.fit
for (i in 1:2) print(Box.test(dat1.fit$residuals,lag = 6*i))#对残差序列进行白噪声检验
dat1.fore <- forecast(dat1.fit,h=5)
plot(dat1.fore)
############################5.2###################
dat2 <- read.table("D:/github_repo/time_series/习题数据、案例数据、R代码/习题数据/习题5.2数据.txt",header=T)
dat2 <- ts(c(dat2[,2],dat2[,4],dat2[,6]))
plot(dat2)#根据时序图来看，序列明显是非平稳的
diff_dat2 <- diff(dat2,1,2)#差分后平稳
plot(diff_dat2)
acf(diff_dat2)#acf拖尾
pacf(diff_dat2)#pacf拖尾，因此可以尝试对差分后的序列拟合arma(1,2,1)模型,根据aic最小原则，可以选择arima(0,2,2)
for (i in 1:2) print(Box.test(diff_dat2,lag = 6*i))#白噪声检验
#二阶差分序列为白噪声序列
auto.arima(diff_dat2)
dat2.fit <- arima(dat2,order = c(0,2,2))
dat2.fit 
for (i in 1:2) print(Box.test(dat2.fit$residuals,lag = 6*i))#对残差序列进行白噪声检验
#(1-B)^2*xt=(1-0.4427*B-0.3315*B^2)*et
#xt=2*xt-1-xt-2+et-0.4427*et-1+0.3315*et-2
y61hat=2*dat2[60]-dat2[59]-0.4427*dat2.fit$residuals[60]+0.3315*dat2.fit$residuals[59]
y61hat
y62hat=2*y61hat-dat2[60]-0.4427*dat2.fit$residuals[60]
y62hat
dat2.fore <- forecast(dat2.fit,h=2)
plot(dat2.fore)
############################5.3###################
rm(list = ls())
dat3 <- read.table("D:/github_repo/time_series/习题数据、案例数据、R代码/习题数据/习题5.3数据.txt",header=T)
dat3 <- c(dat3[,2],dat3[,4],dat3[,6])
dat3 <- ts(dat3,frequency = 12,start = c(2000,1))
plot(dat3)#根据时序图来看，序列明显是非平稳的,存在季节性波动
diff_dat3 <- diff(diff(dat3),12)#1阶12步差分后平稳
plot(diff_dat3)
acf(diff_dat3)#acf拖尾
pacf(diff_dat3)#pacf拖尾
for (i in 1:2) print(Box.test(diff_dat3,lag = 6*i))#白噪声检验
#1阶差分序列d的延迟12阶为白噪声序列
dat3.fit <- arima(dat3,order = c(1,1,1),seasonal = list(order=c(1,1,1 ),period=12))
dat3.fit 
for (i in 1:2) print(Box.test(dat3.fit$residuals,lag = 6*i))#对残差序列进行白噪声检验,不拒绝原假设
dat3.fore <- forecast(dat3.fit,h=3)
plot(dat3.fore)
#####################增加一个garch(1,2)数据模拟的作业#######





###########时间序列分析garch(1，1）模型拟合###############
#读入数据绘制时序图
rm(list = ls())
library(xlsx)
z <- read.xlsx("D:/github_repo/time_series/习题数据、案例数据、R代码/案例数据/附录1.23.xls",1)
#z <- na.omit(z)
z <- ts(z$exchange_rates,start = c(1979,12,31),frequency = 365)
plot(z)
#对差分序列性质考察
plot(diff(z))
acf(diff(z))#截尾
pacf(diff(z))#拖尾
for (i in 1:2) print(Box.test(z,lag = 6*i))#皆显著
#拟合arima模型，提取水平相关信息
z.fit <- auto.arima(z)
#z.fit <- arima(z,order = c(0,1,1))
for (i in 1:6) print(Box.test(z.fit$residuals,lag = i))#残差白噪声检验，皆不能拒绝原假设
#水平预测
z.fore <- forecast(z.fit,h=365)
plot(z.fore)
for (i in 1:6) print(Box.test(z.fit$residuals^2,lag = i))
#拟合GARCH(1,1)模型
library(tseries)
r.fit <- garch(z.fit$residuals,order=c(1,1))
summary(r.fit)
#绘制波动置信区间
r.pred <- predict(r.fit)
plot(r.pred)
#▽xt=et+0.0358et-1+vt vt~N(0,0.0002)
#vt=sqr(ht)*et
#ht=0.9144*ht-1 +0.07617(vt-1)^2
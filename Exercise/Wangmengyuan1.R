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
#因此，二次查分后的序列是白噪声序列，具有纯随机性和平稳性



######################时间序列分析第三次上机实验内容################################



#1.使用arima.sim函数模拟时间序列数据，并作图
x1 <- arima.sim(n=100,list(ar=c(1,-0.5),order(method = "auto")),sd=1)#模拟xt=xt-1-0.5xt-2+et
ts.plot(x1)#可以直观判断时间序列平稳
#2.判断时间序列的平稳性

#3.分别得到3个模型的格林函数的序列（前10项）
#################AR模型##########
#xt=xt-1-0.5xt-2+et,AR(2)格林函数
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
#############MA模型#####################
x2 <- arima.sim(n=1000,list(ma=-2))
acf(x2)
#MA(2)模型逆函数
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
var2 <- 1+theta[1]^2+theta[2]^2#方差
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
#############ARMA(1,1)模型#####################
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
    if(k<=q) theta[k]=theta[k] else theta[k]=0
    vv[k] <- phi[k]*G[j-k]
  }
  G[j] <- sum(vv)-theta[k]
}
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

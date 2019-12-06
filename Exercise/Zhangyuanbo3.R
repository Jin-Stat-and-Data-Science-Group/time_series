#################第三次上机实验##################
rm(list = ls())
set.seed(11)
x1 <- arima.sim(n=200,list(ar=0.6),sd=0.5)
ts.plot(x1)
acf(x1)#拖尾
pacf(x1)#1阶截尾
#可判断为平稳序列

#####写出格林函数前10项
#AR(1)模型
G1 <- 1
phi.1 <- 0.6
for (i in 1:10)  {
  G1[i+1] <- phi.1^i
}

#AR(2)模型
set.seed(22)
x2 <- arima.sim(n=200,list(ar=c(1.2,-0.5)),sd=1.5)
G2 <- c(1,1.2)
phi.1 <- 1.2
phi.2 <- -0.5
for (i in 3:11)  {
  G2[i] <- phi.1*G2[i-1]+phi.2*G2[i-2]
}

#AR(3)模型
set.seed(33)
#特征函数为(x-0.5)(x+0.2)(x-0.4)=0
library(Ryacas)
yacas("Expand((x-0.4)*(x-0.5)*(x+0.2))")
x3 <- arima.sim(n=200,list(ar=c(0.7,-0.02,-0.04)),sd=1)
phi.3 <- c(0.7,-0.02,-0.04)
G3 <- c(1,0.7,0.7*0.7-0.02*1)
for (i in 3:10)  {
  G3[i+1] <- sum(phi.3*G3[i:(i-2)])
}

##############求方差
v.G1 <- sum(G1^2*0.25)
v.G2 <- sum(G2^2*2.25)
v.G3 <- sum(G3^2*1)

#############计算自协方差函数（前10项）
#AR(1)
gamma.G1 <- c()
for(i in 1:10){
  gamma.G1[i]=0.6^i*0.5^2/(1-0.6^2)
}

#AR(2)
gamma.G2 <- c()
phi.1 <- 1.2
phi.2 <- -0.5
gamma.G2[1] <- phi.1*v.G2/(1-phi.2)
gamma.G2[2] <- phi.1*gamma.G2[1]+phi.2*v.G2
for(i in 3:10){
  gamma.G2[i]=phi.1*gamma.G2[i-1]+phi.2*gamma.G2[i-2]
}

#AR(3)
gamma.G3 <- c()
phi.1 <- 0.7
phi.2 <- -0.02
phi.3 <- -0.04
gamma.G3[1] <- (phi.1+phi.2*phi.3)*v.G3/(1-phi.2-phi.1*phi.3-phi.3^2)
gamma.G3[2] <- (phi.1+phi.3)*gamma.G3[1]+phi.2*v.G3
gamma.G3[3] <- phi.1*gamma.G3[2]+phi.2*gamma.G3[1]+phi.3*v.G3
for(i in 4:10){
  gamma.G3[i] <- phi.1*gamma.G3[i-1]+phi.2*gamma.G3[i-2]+phi.3*gamma.G3[i-3]
}

###############计算自相关系数（前10项）
p.G1 <- gamma.G1/v.G1
p.G2 <- gamma.G2/v.G2
p.G3 <- gamma.G3/v.G3
#函数检验，基本一致
acfx1 <- ARMAacf(ar=0.6, lag.max = 10)
acfx2 <- ARMAacf(ar=c(1.2,-0.5), lag.max = 10)
acfx3 <- ARMAacf(ar=c(0.7,-0.02,-0.04), lag.max = 10)

#################第四次上机实验##################
#ARMA(4,3)转化为AR(∞) 求前100项
rm(list = ls())
phi <- c(0.1,0.2,-0.3,0.4)
theta <- c(0.6,0.7,0.8)
I0 <- 1
I1 <- theta[1]
I2 <- theta[1]*I1+theta[2]
I <- c(I0, I1, I2)
for (i in 4:100) {
  I[i] <- sum(theta*I[(i-1):(i-3)])
}
I[2:5] <- I[2:5]-phi
I
#ARMA(4,3)转化为MA(∞) 求前100项
G0 <- 1
G1 <- phi[1]
G2 <- phi[1]*G1+phi[2]
G3 <- phi[1]*G2+phi[2]*G1+phi[3]
G<-c(G0,G1,G2,G3)
for (i in 5:100)  {
  G[i]<-sum(phi*G[(i-1):(i-4)])
}
G[2:4]<-G[2:4]-theta
G

#####写出MA(4)的ACF,根为0.2，0.3,0.6，0.9
library(Ryacas)
yacas("Expand((x-0.2)*(x-0.3)*(x-0.6)*(x-0.9))")
theta <- c(-2,1.35,-0.36,0.0324)
MA.ACF <- c()
ACF0 <- 1
ACF4 <- theta[4]/(1+sum(theta^2))
MA.ACF[c(1,5)] <- c(ACF0,ACF4)
for( k in 2:4){
  MA.ACF[k]=(theta[k-1]+sum(theta[1:(5-k)]*theta[k:4]))/(1+sum(theta^2))
}
ARMAacf(ma=theta, lag.max = 4)#检验,结果一致

#####写出AR(5)的PACF，根为0.25，0.5，0.6，0.75，0.8
yacas("Expand((x-0.25)*(x-0.5)*(x-0.6)*(x-0.75)*(x-0.8))")
phi <- c(2.9,-3.2675,1.77625,-0.46125,0.045)
AR.ACF <- ARMAacf(ar=phi,lag.max = 6)
shift <- function(v,k){
  if (k==0) v else c(tail(v,k),head(v,-k))
}
shift <- Vectorize(shift, "k")
AR.PACF <- function(k){
  v <- AR.ACF[1:k]
  temp <- shift(v, 0:(k-1)) #自相关系数矩阵
  temp[upper.tri(temp)] <- 0 #化为下三角矩阵
  D <- temp+t(temp) #化为对称矩阵
  diag(D) <- 1  
  Dk = D
  Dk[,k] <-AR.ACF[2:(k+1)] 
  AR.PACF <- det(Dk)/det(D)
  print(AR.PACF)
}
for (i in 1:5) AR.PACF(i) #AR(5)的PACF
ARMAacf(ar=phi, lag.max = 5, pacf = T) #检验



overshort <- read.csv("C:/附录1.9.csv")
overshort <- ts(overshort[,2])
#MA(1)
fm1 <- arima(overshort,order=c(0,0,1))
a1 <- -2*fm1$loglik+2*3
b1 <- -2*fm1$loglik+log(57)*3
#ARMA(1,1)
fm2 <- arima(overshort,order=c(1,0,1),include.mean = F)
a2 <- -2*fm2$loglik+2*3
b2 <- -2*fm2$loglik+log(57)*3
#模型选择
#MA(1)模型的AIC和BIC均比ARMA(1,1)的要小，因此选择MA(1)

#ARMA(1,1)预测
eps <- rep(0,60)
e <- rep(0,60)
for(i in 2:57){
  e[i]=-0.0742*e[i-1]+eps[i]-0.6169*eps[i-1]
  eps[i]=overshort[i]-e[i]
}
e[58] <- -0.0742*e[57]-0.6169*eps[57]
e[59] <- -0.0742*e[58]
e[60] <- -0.0742*e[59]
green <- ARMAtoMA(ar=-0.0742,ma=-0.6169,lag.max=10)
v <- vector()
v[1] <- fm2$sigma2
v[2] <- (1+green[1]^2)*fm2$sigma2
v[3] <- (1+green[1]^2+green[2]^2)*fm2$sigma2
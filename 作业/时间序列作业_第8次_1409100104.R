#第四题
library(FinTS)
library(tseries)
#读取数据，画图
x<-read.csv(file="C:/Book2.csv")
birth<-ts(x,start=1750)
birth
plot(birth)
acf(birth)
pacf(birth)
Box.test(birth,type="Lj")
#提取残差，检验异方差性
fm1<-arima(birth,order=c(1,0,1))
birth.res<-resid(fm1)
#arch检验
p1<-rep(0,10)
for(i in 1:10)p1[i]<-Box.test(birth.res,type="Lj",lag=i)$p.value
p1
plot(birth.res^2)
p2<-rep(0,10)
for(i in 1:10)p2[i]<-Box.test(birth.res^2,type="Lj",lag=i)$p.value
#拟合garch(0,1)模型
ArchTest(birth.res,lags=i)
acf(birth.res^2)
pacf(birth.res^2)
fm2<-garch(birth.res,order=c(0,1))

summary(fm2)
#第五题
#读取数据，画图
n<- read.table("C:/习题5.5数据.txt")
n<- as.vector(t(as.matrix(number)))
n<- ts(number,start = 1867)
plot(n)
#拟合残差自回归模型
t <- c(1:length(n))
n.m1 <- lm(n~t)
summary(n.m1)
r2 <- n.m1$residuals
acf(r2)
pacf(r2)
n.m2 <- arima(r2,order=c(2,0,0),include.mean = F)

#习题5.6
#读取数据
interest <- read.table("C:/习题5.6数据.txt")
interest <- as.vector(t(as.matrix(interest)))
interest <- ts(interest,frequency = 12,start=c(1969,1))
plot(interest)
#拟合模型
interest1 <- diff(interest)
plot(interest1)
acf(interest1)
pacf(interest1)
i.m1 <- arima(interest,order=c(1,1,1))
#检验方差齐性
r3 <- i.m1$residuals
plot(r3^2)
#arch检验
p3 <- rep(0,6)
for(k in 1:6){
  p3[k] <- Box.test(r3^2,lag=k)$p.value
}
#拟合garch(2,2)
i.m2 <- garch(r3,order = c(2,2))
summary(i.m2)
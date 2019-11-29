#5.1
w<-scan("C:/习题5.1数据.TXT")
x<-ts(w)
plot(x)
#序列不平稳
x.dif1<-diff(x)
plot(x.dif1)
#经过一阶差分后数列趋于平稳
#白噪声检验
Box.test(x.dif1)
#p值为0.1114，大于0.05，不拒绝原假设，所以差分后序列为白噪声序列
acf(x.dif1)#1阶截尾
pacf(x.dif1)#拖尾 该模型为MA(1)
fm1<-arima(x,order=c(0,0,1))
fm1
for(i in 1:2) 
print(Box.test(fm1$residuals),lag=6*i)
#预测
x108<-x[107]-0.1549*fm1$residuals[107]
x108
x109<-x108
x109
library(forecast)
x.fore<-forecast(fm1)
x.fore

#5.2
w<-scan("C:/习题5.2数据.txt")
x<-ts(w,start=1949)
plot(x)#不平稳
x.dif1<-diff(x)
plot(x.dif1)
x.dif2<-diff(x,lag=2)#2阶差分
plot(x.dif2)
acf(x.dif2)
pacf(x.dif2)
#白噪声检验
for(i in 1:6) print(Box.test(x.dif2,lag=i))
#模型拟合
fm1<-arima(x,order=c(1,2,1))
fm1
for(i in 1:6) print(Box.test(fm1$residual,type="Ljung-Box",lag=i))
#预测
x61<-2.4197*x[60]-1.8394*x[59]+0.4197*x[58]-0.8958*fm1$residual[60]
x61
x62<-2.4197*x61-1.8394*x[60]+0.4197*x[59]        
x62
x.fore<-forecast(fm1)
x.fore
plot(x.fore)

#5.3
w<-read.table("C:/习题5.3数据.txt")
x<-ts(w,start=c(1973,1),frequency = 12)
plot(x)#不平稳
x.dif1<-diff(x)
plot(x.dif1)
acf(x.dif1)
pacf(x.dif1)
#白噪声检验
for(i in 1:6) print(Box.test(x.dif1,lag=i))
# 模型拟合
fm1= arima(data3,order=c(1,1,1))
fm1
# 白噪声检验
for (i in 1:6) {
  print(Box.test(fm1$residual,type="Ljung-Box",lag=i))
}
# 预测
x.fore3 = forecast(fm1,h=2)
x.fore
plot(x.fore)

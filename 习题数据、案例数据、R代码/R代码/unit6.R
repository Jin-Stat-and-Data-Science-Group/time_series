#unit 6

#例6-1
#读入数据，并绘制输出序列时序图
a<-read.table("E:/R/data/file24.csv",sep=",",header = T)
y<-ts(a$output)
plot(y)
#考察输出序列自相关图和偏自相关图
acf(y)
pacf(y)
#对输出序列拟合模型AR(4)
y.fit<-arima(y,order=c(4,0,0))
y.fit
#残差白噪声检验
for (i in 1:2) print(Box.test(y.fit$residual,lag=6*i))
x<-ts(a$input)
library(TSA)
y.fit2<-arima(y,order=c(4,0,0),xtransf = x,transfer = list(c(2,1)))
y.fit2

#例6-2
#拟合带漂移项的差分平稳序列，并绘制时序图和线性拟合效果图
e<-rnorm(1000)
x<-filter(0.1+e,filter = 1,method = "recursive")
plot(x)
t<-c(1:1000)
abline(lm(x~t),col=2)
lm(x~t)
#差分运算和线性拟合后残差序列比较
r1<-diff(x)
x.fit<-lm(x~t)
r2<-ts(x.fit$residual)
c1<-min(r1,r2)
c2<-max(r1,r2)
plot(r1,ylim=c(c1,c2))
lines(r2,col=2)

#例6-3
#拟合趋势平稳序列，并绘制时序图
t<-c(1:1000)
e<-rnorm(1000,0,10)
x<-0.1*t+e
x<-ts(x)
plot(x)
#考察差分后差分序列的波动范围
plot(diff(x))
abline(h=c(1.98*sd(diff(x)),-1.98*sd(diff(x))),col=2)
#考察线性拟合后残差序列的波动范围
x.fit<-lm(x~t)
r<-ts(x.fit$residual)
plot(r)
abline(h=c(1.98*sd(r),-1.98*sd(r)),col=2)

#例6-4
#读入数据，并绘制两序列时序图
b<-read.table("E:/R/data/file25.csv",sep=",",header = T)
x<-ts(b$lnx,start = 1978)
y<-ts(b$lny,start = 1978)
c1<-min(x,y)
c2<-max(x,y)
plot(x,ylim=c(c1,c2))
lines(y,lty=2,col=2)
#对人均纯收入对数序列进行DF检验
library(fBasics)
library(fUnitRoots)
adfTest(x,lag=1,type="nc")
adfTest(x,lag=1,type="c")
adfTest(x,lag=1,type="ct")
#对人均生活消费支出对数序列进行DF检验
adfTest(y,lag=1,type="nc")
adfTest(y,lag=1,type="c")
adfTest(y,lag=1,type="ct")

#例6-4续(1)
#差分运算
dx<-diff(x)
dy<-diff(y)
#人均收入对数差分后序列ADF检验
for (i in 1:3) print(adfTest(dx,lag=i,type='nc'))
for (i in 1:3) print(adfTest(dx,lag=i,type='c'))
#人均支出对数差分后序列ADF检验
for (i in 1:3) print(adfTest(dy,lag=i,type='nc'))
for (i in 1:3) print(adfTest(dy,lag=i,type='c'))

#例6-4续(2)
#第一步:构造回归模型
y.fit<-lm(y~x)
summary(y.fit)
#第二步:残差序列单位根检验
r<-ts(y.fit$residual)
for (i in 1:3) print(adfTest(r,lag=i,type='nc'))

#例6-4续(3)
ECM<-y.fit$residual[1:24]
dify.fit<-lm(diff(y)~0+diff(x)+ECM)
summary(dify.fit)

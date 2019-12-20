#第一次上机实验
rm(list = ls())
setwd("D:/github_repo/time_series/习题数据、案例数据、R代码/习题数据")
#导入数据，生成向量，生成时间序列
library(zoo)
a=read.table('./习题2.2数据.txt')
a=as.vector(a)
a=ts(a,frequency = 4,start=c(1949,1))

#子集提取
##提取前10年数据，并生成gdpb78.csv文件
gdpb78=window(a,start=c(1949,1),end=c(1958,1),frequency=4)

##填补缺失值，生成gdpB.csv文件
gdpB=window(a,start=c(1949,1),end=c(1970,4),frequency=4,extend=TRUE)
###线性插值
gdpB=na.approx(gdpB)
###样条插值
#gdpB=na.spline(gdpB)

#第二次上机实验

#实验一利用习题2.2数据

co2=read.table('./习题2.2数据.txt')
co2=as.vector(as.matrix(co2))
co2=ts(co2,frequency = 12,start=c(1975,1))
#绘制时间序列图，判断时间序列是否平稳
plot(co2,xlab="time",ylab="co2",type="l")
#数据呈现周期性波动，故非平稳

#计算时间序列的样本自相关系数pk（k=1,2,…，24）;绘制样本自相关图，并解释该图形
auto_acf=function(ts,k){
  ts_mean_hat=mean(ts)
  n=length(ts);auto_acf_acf=vector()
  ts_var=sum((ts-ts_mean_hat)^2)
  for(i in 1:k){
    a=0
    for(t in 1:(n-i)) {
      a=a+(ts[t]-ts_mean_hat)*(ts[t+i]-ts_mean_hat)
    }
    p=a/ts_var
    auto_acf_acf[i]=p
  }
  return(auto_acf_acf)
}

auto_acf(co2,24)  
acf(co2,24)$acf
plot(auto_acf(co2,24),type = 'h',xlab='acf',ylab='lag')
abline(h=0)
#样本自相关系数呈周期性变化，且只有少数阶数的自相关系数在2倍标准差以内，说明该时间序列非平稳且具有自相关性


#实验二利用习题2.3数据
rain=read.table("./习题2.3数据.txt")
rain=as.vector(as.matrix(rain))
rain=ts(rain,frequency=12,start=c(1945))

plot(auto_acf(rain,24),type = 'h',xlab='acf',ylab='lag')
abline(h=0)

#计算时间序列的样本自相关系数
auto_acf(rain,24)
acf(rain1,lag=24)$acf
#平稳性判断：样本自相关系数在滞后1阶和滞后5阶超过2倍标准差以内，之后迅速衰减到0，具有短期相关性，该序列是平稳序列。

#判断时间序列的纯随机性：白噪声检验
auto_box_test=function(ts,lag,type){
  n=length(ts);m=lag;lbox=0
  if (type=='Ljung'){
    for(k in 1:m){
      lbox=lbox+((auto_acf(ts,m)[k])^2)/(n-k)
    }
    QB=n*(n+2)*lbox
    P=(1-pchisq(QB,df=m))
    value=matrix(c(QB,P,m),1,3)
    colnames(value)=c('LB统计量','p-value','df')
    value
  }
  else{
    Q=n*sum(auto_acf(ts,m)^2)
    P=(1-pchisq(Q,df=m))
    value=matrix(c(Q,P,m),1,3)
    colnames(value)=c('Q统计量','p-value','df')
    value
  }
  }
auto_box_test(rain,lag=10,type='Ljung')
Box.test(rain,lag = 10,type = "Ljung")

#表明该序列为白噪声序列


#实验三利用习题2.5数据
sale=read.table("./习题2.5数据.txt",header=TRUE)
sale=as.vector(as.matrix(sale[,2:5]))
sale=ts(sale,frequency=12,start=c(2000,1))

#判断序列{xt}的平稳性及纯随机性；
auto_acf(sale,24)
acf(sale,lag=24)$acf
plot(auto_acf(sale,24),type = 'h',xlab='acf',ylab='lag',ylim = c(-1,1))
abline(h=c(0,-1.96*1/sqrt(length(sale)),1.96*1/sqrt(length(sale))),lty=2)
acf(sale,lag=24)#周期性波动明显，非平稳序列

auto_box_test(sale,lag=10,type='Ljung')
Box.test(sale,lag = 10,type = "Ljung")
#该序列具有相关性

#对序列进行函数运算：yt=xt-xt-1，并判断序列{yt}的平稳性及纯随机性
dsale=diff(sale)
auto_acf(dsale,24)
acf(dsale,lag=24)$acf
plot(auto_acf(dsale,24),type = 'h',xlab='acf',ylab='lag',ylim = c(-1,1))
abline(h=c(0,-1.96*1/sqrt(length(sale)),1.96*1/sqrt(length(sale))),lty=2)
acf(dsale,lag=24)#周期性波动明显，非平稳序列

auto_box_test(dsale,lag=10,type='Ljung')
Box.test(dsale,lag = 10,type = "Ljung")
#该序列具有相关性

######################时间序列分析第三次上机实验内容################################
#1.使用arima.sim函数模拟时间序列数据，并作图
#2.判断时间序列的平稳性
#3.分别得到3个模型的格林函数的序列（前10项）
#4.得到3个模型的方差
#5.计算自协方差函数（前10项）
#6.计算自相关系数（前10项）


################################ 拟合ARMA模型  ##########################
#xt=xt-0.5*x(t-1)-0.4*x(t-2)+et-0.1*e(t-1)-0.3*e(t-2),AR(2)格林函数
arma.sim=function(n,model,sd){
  x=vector(length = n)
  p=length(model[[1]])
  q=length(model[[2]])
  if (abs(model[[1]][2])>1 || sum(model[[1]])>=1 ||((model[[1]][1])-(model[[1]][2]))>=1){
    return('序列不平稳')
  }
  if (abs(model[[2]][2])>1 || sum(model[[2]])>=1 ||((model[[2]][1])-(model[[2]][2]))>=1){
    return('序列不可逆')
  }
  n.start=p+q
  set.seed(1234)
  start.innov=rnorm(n.start,mean=0,sd=sd)
  innov=rnorm(n-2)
  e=ts(c(start.innov[c(3,4)], innov[1L:(n-2)]))
  x[1]=start.innov[1];x[2]=start.innov[2]
  
  for (i in 3:n){
    x[i]=sum(c(model[[1]])*c(x[i-1],x[i-2]))-sum(c(model[[2]])*c(e[i-1],e[i-2]))+e[i]
  }
  return(ts(x))
}   
B=arma.sim(n=1000,model=list(ar=c(-0.5,-0.4),ma=c(0.1,0.3)),sd=1)
library(forecast)
plot(B,lty=1,type='l')
arima(B,include.mean = FALSE,order = c(2,0,2))
auto.arima(B)
################################ AR模型格林函数AR(2)(0.7，0.1) ##########################
phi=c(0.7,0.1);phii=vector();g=vector()
for(k in 1:100){
  if(k<=2){
    phii[k]=phi[k]
  }
  if(k>2){
    phii[k]=0
  }
}

g[1]=1
for(i in 2:10){
  g[i]=sum(phii[1:(i-1)]*g[c(seq((i-1),1,-1))])
}

g
#############################拟合ma模型##################################################
theta=c(0.4,0.3);x=c()
e=c(0,0,rnorm(100))
for(i in 1:100){
  x[i]=e[i+2]-theta[1]*e[i+1]-theta[2]*e[i]
}

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
setwd('D:/github_repo/time_series/习题数据、案例数据、R代码/习题数据')
#习题5.1
data1=read.table("习题5.1数据.txt",fill=T)

#1.将数据转化为序列
data1=as.vector(t(as.matrix(data1)))
data1=na.omit(data1)
data1=ts(data1,frequency=12)

#2.画时序图观测特征，判断序列是否平稳
plot(data1,ylab="收盘价",type = "l")#该序列非平稳

#3.做差分，画差分时序图
data1_1=diff(data1,differences=1)
data1_2=diff(data1,differences=2)
plot(data1_1,ylab="一阶差分收盘价",type = "l")
plot(data1_2,ylab="二阶差分收盘价",type = "l")
library(tseries)
pp.test(data1_1)#p-value =0.01，表明一阶差分收盘价序列平稳

#4.检验数据是不是白噪声
auto_box_test(data1_1,lag=10,type='Ljung')
auto_box_test(data1_2,lag=10,type='Ljung')
Box.test(data1_1,lag=10,type="Ljung-Box")#p-value = 0.27,表明一阶差分序列为白噪声序列
Box.test(data1_2,lag=10,type="Ljung-Box")#p-value = 8.155e-07，二阶差分序列非白噪声序列

#5.建立arima模型，模型诊断
plot(auto_acf(data1_1,24),type = 'h',xlab='acf',ylab='lag',ylim = c(-1,1))
abline(h=c(0,-1.96*1/sqrt(length(data1_1)),1.96*1/sqrt(length(data1_1))),lty=2)#均在两倍标准差以内
acf(data1_1)
pacf(data1_1)#尝试建立arima(0,1,1)
fit0=arima(data1,order=c(0,1,1))#aic = 574.89
fit0
library(forecast)
fit=auto.arima(data1)#构建ARIMA(0,1,0)，AIC=575.39
fit

#6.对残差序列进行白噪声检验
qqnorm(fit$residuals)#如果数据满足正态分布，则数据中的点会落在图中的线上
qqline(fit$residuals)
Box.test(fit$residuals,lag=10,type="Ljung-Box")#p-value = 0.2576，残差序列为白噪声序列

#7.模型预测
forecast(fit,h=3)
plot(forecast(fit,3))

#习题5.2
#1.将数据转化为序列
data2=read.table("习题5.2数据.txt",header = TRUE)
data2=ts(as.vector(as.matrix(data2[,c(2,4,6)])),frequency=1,start = c(1949))


#2.画时序图观测特征，判断序列是否平稳
plot(data2,ylab="y",type = "l")#该序列非平稳,有增长趋势

#3.做差分，画差分时序图
data2_1=diff(data2,differences=1)
data2_2=diff(data2,differences=2)
plot(data2_1,type = "l")
plot(data2_2,type = "l")#二阶差分好一些


#4.检验数据是不是白噪声
for(i in 1:6) print(Box.test(data2_1,lag=i))#p-value =2.2e-16,表明一阶差分序列为非白噪声序列
for(i in 1:6) print(Box.test(data2_2,lag=i))#p-value >0.05,表明二阶差分序列为白噪声序列
#5.对差分数据进行识别
plot(auto_acf(data2_1,24),type = 'h',xlab='acf',ylab='lag',ylim = c(-1,1))
abline(h=c(0,-1.96*1/sqrt(length(data2_1)),1.96*1/sqrt(length(data2_1))),lty=2)
#自相关系数一阶截尾
acf(data2_1)
pacf(data2_1)#偏相关系数均在两倍标准差范围内，尝试建立arima(1,1,0),arima(1,1,1)

#6.建立arima模型
library(forecast)
fit=arima(data2,order=c(1,1,0))
fit#aic = 1226
fit1=arima(data2,order=c(1,1,1))
fit1#aic =1228
fit2=auto.arima(data2)#arima(1,2,1)
fit2#aic = 1204

#7,对残差序列进行白噪声检验
qqnorm(fit2$residuals)#如果数据满足正态分布，则数据中的点会落在图中的线上
qqline(fit2$residuals)
for(i in 1:6) print(Box.test(fit2$residuals,lag=i))#p-value = 0.038，残差序列不是白噪声序列,需继续提取序列信息

qqnorm(fit$residuals)#如果数据满足正态分布，则数据中的点会落在图中的线上
qqline(fit$residuals)
for(i in 1:6) print(Box.test(fit$residuals,lag=i))#白噪声序列
#模型预测
forecast(fit2,3)
plot(forecast(fit2,3))


#习题5.3(需要先提取周期性因素)
#1.将数据转化为序列
data3.1=read.table('习题5.3数据.txt',header=TRUE)
data3.2=ts(as.vector(as.matrix(data3.1[,c(2,4,6)])),frequency=12,start=c(1973,1))


#2.画时序图观测特征，判断序列是否平稳
plot(data3.2,ylab="死亡人数",type = "l")#该序列非平稳,有周期性波动


#3.做差分，画差分时序图
data3.3=diff(diff(data3.2),lag=12)#一阶12步差分
plot(data3.3,type = "l")

pp.test(data3.3)#p-value =0.01，表明一阶差分序列平稳

#4.检验数据是不是白噪声
for(i in 1:6) print(Box.test(data3.3,lag=i))#为非白噪声序列

#5.对差分数据进行识别,建立乘法模型
acf(data3.3,lag=24)$acf
#样本自相关系数具有周期性特征，1阶，12阶在两倍标准差以外
pacf(data3.3,lag=24)$pacf
#偏自相关系数具有周期性，1阶，12阶在两倍标准差以外，尝试对季节模型构建ARMA(1,1)/ARMA(1,2)/ARMA(2,2)/ARMA(2,1)
#6.建立arima模型
#看一个周期内的短期相关性可以发现，短期自相关系数1阶截尾，短期偏自相关系数1阶截尾,故对短期相关性用arma(1,1)拟合
library(forecast)
pra_order1=list(c(1,1,0),c(1,1,1),c(0,1,1),c(0,1,0))
pra_order2=list(c(1,1,1),c(1,1,2),c(0,1,1),c(1,1,0),c(0,1,0))
pra=expand.grid(pra_order1,pra_order2)
fit_aic=vector()
for (i in 1:length(pra$Var1)){
  fit=arima(data3.2,order=c(pra$Var1[i][[1]]),seasonal = list(order=c(pra$Var2[i][[1]]),period=12))
  fit_aic[i]=fit$aic
}
which(fit_aic==min(fit_aic))#第三个模型最好。即arima(0,1,1)(0,1,1)[12]
pra[11,]
fit3=auto.arima(data3.2)#验证上面的arima(0,1,1)(0,1,1)[12]乘法模型最好
fit3#aic = 705.37
#构建ARIMA(2,1,2)，AIC=644.76


#7.对残差序列进行白噪声检验
fit_max=arima(data3.2,order=c(pra$Var1[11][[1]]),seasonal = list(order=c(pra$Var2[11][[1]]),period=12))
qqnorm(fit_max$residuals)#如果数据满足正态分布，则数据中的点会落在图中的线上
qqline(fit_max$residuals)
Box.test(fit_max$residuals,lag=10,type="Ljung-Box")#p-value = 0.468，残差序列是白噪声序列

#第八次作业
#1.模型预测
forecast(fit_max,3)
plot(forecast(fit_max,3))

#garch(1,2)模型模拟
beta=c(0.4,0.3);alpha=0.1;h=c();ver=c()
e=c(rnorm(1000));omiga=0.5
h[1]=h[2]=0.01
ver[1]=sqrt(h[1])*e[1];ver[2]=sqrt(h[2])*e[2]
for(i in 3:1000){
  h[i]=omiga+alpha[1]*h[i-1]+beta[1]*(ver[i-1])^2+beta[2]*(ver[i-2])^2
  ver[i]=sqrt(h[i])*e[i]
}
plot(ts(ver))
library(FinTS)
ArchTest(ver,12)
#2.习题5.4
##拟合序列发展
data4=read.table('习题5.4数据.txt',header=TRUE)
data4=ts(as.vector(as.matrix(data4[,c(2,4,6,8)])),frequency=1,start=c(1750))
plot(data4)#平稳
for(i in 1:6) print(Box.test(data4,lag=i))#为非白噪声序列

plot(auto_acf(data4,24),type = 'h',xlab='lag',ylab='acf',ylim = c(-0.2,1))
abline(h=c(0,-1.96*1/sqrt(length(data4)),1.96*1/sqrt(length(data4))),lty=2)#自相关系数一阶截尾
acf(data4)
pacf(data4)#偏自相关系数一阶截尾

fit_level=arima(data4,order=c(1,0,1))
plot(fit_level$residuals)#显示残差序列有异方差性
for(i in 1:6) print(Box.test(fit_level$residuals),lags=i)#PQ检验显示残差序列无相关性
for(i in 1:6) print(Box.test((fit_level$residuals)^2,lag=i))#PQ检验为非白噪声序列，存在异方差
for(i in 1:6) print(ArchTest(fit_level$residuals),lags=i)#LM检验为非白噪声序列，存在异方差

plot(auto_acf(fit_level$residuals^2,24),type = 'h',xlab='lag',ylab='acf',ylim = c(-0.2,1))
abline(h=c(0,-1.96*1/sqrt(length(fit_level$residuals^2)),1.96*1/sqrt(length(fit_level$residuals^2))),lty=2)
#自相关系数一阶截尾
acf(fit_level$residuals^2)
pacf(fit_level$residuals^2)#偏自相关系数二阶截尾

#garch(1,1)
fit_resi1=garch(fit_level$residuals,order = c(1,1))
plot(fit_resi1$residuals)
summary(fit_resi1)#最终的残差序列不服从正态分布，且存在相关性
#garch(1,2)
fit_resi2=garch(fit_level$residuals,order = c(1,2))
plot(fit_resi2$residuals)
summary(fit_resi2)#最终的残差序列不服从正态分布，且存在相关性

#garch(2,2)
fit_resi3=garch(fit_level$residuals,order = c(2,2))
plot(fit_resi3$residuals)
summary(fit_resi3)#最终的残差序列不服从正态分布，且存在相关性
for(i in 1:6) print(Box.test(fit_resi3$residuals),lags=i)#PQ检验显示残差序列无相关性
for(i in 1:6) print(ArchTest(fit_resi3$residuals),lags=i)#LM检验显示异方差

#garch(2,1)
fit_resi4=garch(fit_level$residuals,order = c(2,1))
plot(fit_resi4$residuals)
summary(fit_resi4)#最终的残差序列不服从正态分布，且存在相关性
for(i in 1:6) print(Box.test(fit_resi4$residuals),lags=i)#PQ检验显示残差序列无相关性
for(i in 1:6) print(ArchTest(fit_resi4$residuals),lags=i)#LM检验显示存在异方差

#garch(0,1)为最优模型
fit_resi5=garch(fit_level$residuals,order = c(0,1))
plot(fit_resi5$residuals)
summary(fit_resi5)
for(i in 1:6) print(Box.test(fit_resi5$residuals),lags=i)#PQ检验显示残差序列无相关性
for(i in 1:6) print(ArchTest(fit_resi5$residuals),lags=i)#LM检验显示方差齐性
 
#garch(0,2)
fit_resi6=garch(fit_level$residuals,order = c(0,2))
plot(fit_resi6$residuals)
summary(fit_resi6)#最终的残差序列不服从正态分布，且存在相关性
for(i in 1:6) print(Box.test(fit_resi6$residuals),lags=i)#PQ检验显示残差序列无相关性
for(i in 1:6) print(ArchTest(fit_resi6$residuals),lags=i)#LM检验显示存在异方差

#习题5.5
data5<- read.table('习题5.5数据.txt')
data5=ts(as.vector(t(as.matrix(data5))),frequency=1,start=c(1867))
plot(data5)#非平稳
plot(diff(data5))
data5_1=diff(data5)
for(i in 1:6) print(Box.test(data5_1,lag=i))#为非白噪声序列

plot(auto_acf(data5_1,24),type = 'h',xlab='lag',ylab='acf',ylim = c(-0.2,1))
abline(h=c(0,-1.96*1/sqrt(length(data5_1)),1.96*1/sqrt(length(data5_1))),lty=2)#自相关系数拖尾
acf(data5_1)
pacf(data5_1)#偏自相关系数拖尾

fit=arima(data5,order=c(1,1,1))
summary(fit)

fit=auto.arima(data5)
summary(fit)#ARIMA(2,1,1) 

plot(fit$residuals)
for(i in 1:6) print(Box.test(fit$residuals),lags=i)#PQ检验显示残差序列无相关性
for(i in 1:6) print(Box.test((fit$residuals)^2,lag=i))#PQ检验为方差齐性
for(i in 1:6) print(ArchTest(fit$residuals),lags=i)#LM检验为方差齐性

#习题5.6
data6<- read.table('习题5.6数据.txt')
data6<- as.vector(t(as.matrix(data6)))
data6<- ts(data6,start =c(1969,1),frequency = 12)
plot(data6)#序列可能有周期性
data6_1 <- diff(data6)
plot(data6_1)#1阶差分后序列趋于平稳，但方差非齐性

#ARIMA(1,1,1) 
fit_level=auto.arima(data6)

for(i in 1:6) print(Box.test(fit_level$residuals,type="Lj",lag=i))#序列无自相关性
for(i in 1:6) print(Box.test((fit_level$residuals)^2,lag=i))#PQ检验异方差
for(i in 1:6) print(ArchTest(fit_level$residuals),lags=i)#LM检验异方差

#拟合garch(0，2)
fit <- garch(fit_level$residuals,order = c(0,1))
AIC(fit)
fit <- garch(fit_level$residuals,order = c(0,2))
AIC(fit)#aic最小
fit <- garch(fit_level$residuals,order = c(1,2))
AIC(fit)
fit <- garch(fit_level$residuals,order = c(1,1))
summary(fit);AIC(fit)
fit <- garch(fit_level$residuals,order = c(1,0))
AIC(fit)

plot(fit$residuals)
for(i in 1:6) print(Box.test(fit$residuals),lags=i)#PQ检验显示残差序列无相关性
for(i in 1:6) print(Box.test((fit$residuals)^2,lag=i))#PQ检验为方差齐性
for(i in 1:6) print(ArchTest(fit$residuals),lags=i)#LM检验为方差齐性


#第9次作业
#习题6.2
library(fBasics)
library(fUnitRoots)
data=read.table('习题6.2数据.txt',fill = T)
corn=ts(na.omit(as.numeric(t(as.matrix(data[c(2,3,4,5),])))))
rain=ts(na.omit(as.numeric(t(as.matrix(data[c(7,8,9,10),])))))
##单位根检验平稳性
for(k in 1:3) print(adfTest(corn,type = c("nc"),lag=k))
for(k in 1:3) print(adfTest(corn,type = c("c"),lag=k))
#谷物产量序列是有常数均值的平稳序列
for(l in 1:3) print(adfTest(rain,type = c("nc"),lag=k))
for(k in 1:3) print(adfTest(rain,type = c("c"),lag=k))
#降雨量序列是有常数均值的平稳序列

##白噪声检验
for(k in 1:3) print(Box.test(corn,lag=k))#序列为白噪声
for(l in 1:3) print(Box.test(rain,lag=l))#序列为白噪声

##协整检验
f1 <- lm(corn~rain)
r1 <- ts(f1$residuals)
for(k in 1:3) print(adfTest(r1,type=c("nc"),lag=k))
#残差序列为平稳序列，所以两者之间具有协整关系

#习题6.4
dat2=read.table('习题6.4数据.txt',header = T,sep = "\t",encoding = "UTF-8",skipNul = T)
#data=read.table('习题6.4数据.txt')

export <- ts(dat2[,2],start=1950)
import <- ts(dat2[,3],start=1950)
plot(export)
plot(import)#呈指数趋势

##单位根检验

for(k in 1:3) print(adfTest(export,type = c("nc"),lag=k))
for(k in 1:3) print(adfTest(export,type = c("c"),lag=k))
for(k in 1:3) print(adfTest(export,type = c("ct"),lag=k))
#出口总额序列非平稳

for(k in 1:3) print(adfTest(import,type = c("nc"),lag=k))
for(k in 1:3) print(adfTest(import,type = c("c"),lag=k))#带有均值
for(k in 1:3) print(adfTest(import,type = c("ct"),lag=k))#带有趋势项和均值
#进口总额序列非平稳

##分别对两个序列进行模型拟合
e1 <- log(export)
i1 <- log(import)
plot(e1)
plot(i1)

for(k in 1:3) print(adfTest(e1,type = c("ct"),lag=k))#带有趋势项和均值的ADF检验e1显示不平稳
for(k in 1:3) print(adfTest(i1,type = c("ct"),lag=k))#带有趋势项和均值的ADF检验i1显示不平稳

e1_1=diff(e1,differences=1)
plot(e1_1)
for(k in 1:3) print(adfTest(e1_1,type = c("c"),lag=k))#export带有均值的ADF检验显示一阶差分对数序列平稳

i1_1=diff(i1,differences=1)
plot(i1_2)
for(k in 1:3) print(adfTest(i1_1,type = c("c"),lag=k))#import带有均值的ADF检验显示一阶差分对数序列平稳


t <- c(1:59)
fm2 <- lm(e1~t)#先提取趋势特征后平稳
summary(fm2)
fm3 <- lm(i1~t)
plot(fm2$residuals,type='l')
plot(fm3$residuals,type='l')
##协整检验
fm4 <- lm(e1~i1)
r2 <- fm4$residuals
for(k in 1:3) print(adfTest(r2,type=c("nc"),lag=k)) 
#残差序列为平稳序列，所以两者之间具有协整关系

##残差修正模型
ecm <- fm4$residuals[1:58]
fm5 <- lm(diff(e1)~0+diff(i1)+ecm)
summary(fm5)
plot(fm5$residuals,type='l')
for(k in 1:3) print(adfTest(fm5$residuals,type=c("nc"),lag=k))
for(l in 1:3) print(Box.test(fm5$residuals,lag=6*l))#序列为白噪声


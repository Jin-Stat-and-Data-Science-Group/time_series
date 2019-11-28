#第一次上机实验
rm(list = ls())
setwd("D:/习题数据、案例数据、R代码/习题数据")
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


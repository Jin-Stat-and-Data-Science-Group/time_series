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

rm(list = ls())
setwd("D:/作业/时间序列/习题数据、案例数据、R代码/data/")
#自定义求自相关系数的函数auto_acf
auto_acf <- function(ts,k){
  #ts表示输入的时间序列，k表示滞后阶数
  #输出：时间序列ts的k阶自相关系数
  l <- length(ts)
  ts1 <- ts[1:(l-k)]
  ts2 <- ts[(k+1):l]
  ts_mean <- mean(ts)
  ts_var <- sum((ts-ts_mean)^2)
  auto_acf <- 0
  for (i in 1:length(ts1)){
    temp <- (ts1[i]-ts_mean)*(ts2[i]-ts_mean)/ts_var
    auto_acf <- auto_acf + temp
  }
  return(auto_acf)
}
####实验一
dat1 <- read.csv('file1.csv')
yield <- ts(dat1$yield,start = 1884)#生成时间序列数据
plot(yield)#时序图检验
#通过时序图看出yield大致在16附近随机波动，没有明显趋势或周期，基本可以视为平稳序列。
for (i in 0:12) {print(auto_acf(yield,i))}#按公式计算的自相关系数结果
print(acf(yield,lag=12))#与acf函数计算结果基本一致
acf(yield)#自相关图检验
#从图中可以看出ACF一直在零轴附近波动，稳定在2倍标准差之内，可以认为该序列是平稳的。

####################################################
####实验二
dat2 <- read.csv('file2.csv')
index <- ts(dat2$index,start = 1500)#生成时间序列数据
for (i in 0:12) {print(auto_acf(index,i))}#按公式计算的自相关系数结果
print(acf(index,lag=12))#与acf函数计算结果基本一致
#平稳性检验
plot(index)#时序图检验
#通过时序图看出index具有明显上升趋势,且呈现一定周期性，基本可以视为非平稳序列。
acf(index)#自相关图检验
#从图中可以看出ACF长期位于零轴上方，且ACF都较大，说明该序列具有单调趋势，是非平稳序列。
#纯随机性检验
Box.test(index,lag = 6)
#从结果得到p-value为2.2e-16远小于显著性水平0.05，可拒绝纯随机性的原假设。

####################################################
####实验三
dat3 <- read.csv('file3.csv')
sunsplot <- ts(dat3$sunsplot,start = 1820)#生成时间序列数据
#平稳性检验
plot(sunsplot)#时序图检验
#通过时序图看出sunsplot呈现一定周期性，基本可以视为非平稳序列。
acf(sunsplot)#自相关图检验
#从图中可以看出自相关图呈现明显的正弦波动规律，说明该序列具有周期性，是非平稳序列。
#纯随机性检验
a <- 0
n <- length(sunsplot)
m <- 6 #滞后阶数为6
for (i in 1:m) {
  b <- auto_acf(sunsplot,i)^2/(n-i)
  a <- a + b
}
LB <- n*(n+2)*a
p <- pchisq(LB,6,log.p=T)#p值远小于α，拒绝原假设。
Box.test(sunsplot,lag = 6,type = 'Lj')
#从结果得到p-value为7.139e-14远小于显著性水平0.05，可拒绝纯随机性的原假设。

Yt <- diff(sunsplot)
#平稳性检验
plot(Yt)#时序图检验
#通过时序图看出Yt呈现一定周期性，基本可以视为非平稳序列。
acf(Yt)#自相关图检验
#从图中可以看出自相关图呈现明显的正弦波动规律，说明该序列具有周期性，是非平稳序列。
#纯随机性检验
Box.test(Yt,lag = 6)
#从结果得到p-value为1.607e-07远小于显著性水平0.05，可拒绝纯随机性的原假设。
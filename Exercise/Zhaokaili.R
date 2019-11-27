library(tseries)
setwd("D:/时间序列/习题数据、案例数据、R代码")

#自定义求自相关系数的函数auto_acf
auto_acf <- function(ts,k){
  #ts表示输入的时间序列，k表示滞后阶数
  #输出：时间序列ts的k阶自相关系数
  l <- length(ts)
  ts1 <- ts[1:(l-k)]
  ts2 <- ts[(k+1):l]
  ts_mean <- mean(ts)
  ts_var <- sum((ts-ts_mean)**2)
  auto_acf <- 0
  for (i in 1:length(ts1)){
    temp <- (ts1[i]-ts_mean)*(ts2[i]-ts_mean)/ts_var
    auto_acf <- auto_acf + temp
  }
  return(auto_acf)
}

#实验一
dat1 <- read.csv("data/file2.csv", header=T)
plot(dat1$Year, dat1$index, type='l')
#绘制时序图，该时间序列有明显的递增趋势，不是平稳序列
for (i in 1:24) {print(auto_acf(dat1$index,i))}#按公式计算的自相关系数结果
print(acf(dat1$index,lag=24))#与acf函数计算结果差异很小
acf(dat1$index)#绘制样本自相关图
#该序列的自相关系数在很长的延迟时期都没有递减到0，所以该序列是非平稳序列。

#实验二
dat2 <- read.csv("data/file3.csv", header=T)
plot(dat2$year, dat2$sunsplot, type='l')#该序列具有周期性，不是平稳序列
acf(dat2$sunsplot)#根据自相关图进一步判断不是平稳序列
for (i in 1:24) {print(auto_acf(dat2$sunsplot,i))}#计算时间序列的样本自相关系数pk（k=1,2,…，24）
print(acf(dat2$sunsplot,lag=24))#利用acf函数检查
a <- 0
n <- length(dat2$sunsplot)
m <- 6 #滞后阶数为6
for (i in 1:m) {
  b <- auto_acf(dat2$sunsplot,i)**2/(n-i)
  a <- a + b
}
LB <- n*(n+2)*a
p <- pchisq(LB,6,log.p=T)#p值p值远小于α，拒绝原假设，sunsplot序列属于非白噪声序列。
print(Box.test(dat2$sunsplot, lag=6))#自带函数检查，一致


#实验三
dat3 <- read.csv("data/file4.csv", header=T)
plot(dat3$time, dat3$output, type='l')#该序列随时间呈增长趋势，不属于平稳序列
for (i in 1:2) print(Box.test(dat3$output, lag=i*6))
#纯随机检验结果显示，在各阶延迟下LB检验统计量的p值都非常小，可以认为output序列属于非白噪声序列
Xt <- dat3$output
Yt <- c()
for (i in 2:length(Xt)){
  Yt[i] <- Xt[i] - Xt[i-1]
}
Yt <- na.omit(Yt)
Yt <- ts(Yt, start=1965)
plot(Yt)
acf(Yt)#根据时序图和自相关图，易知一阶差分序列Yt是平稳序列
for (i in 1:2) print(Box.test(Yt, lag=i*6))
#根据纯随机检验，p值分别为0.8137，0.9922远大于显著性水平α，不能拒绝原假设，
#所以认为Yt是白噪声序列


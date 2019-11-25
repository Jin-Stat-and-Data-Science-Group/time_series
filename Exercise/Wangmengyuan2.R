#时间序列分析第二次上机实验内容
#实验一
rm(list = ls())
setwd('D:/时间序列分析/习题数据、案例数据、R代码/')
dat1 <- read.csv("data/file1.csv",header = T)
#变量yield的时间序列图
plot(dat1$yield~dat1$Year,xlab='Year',ylab = 'yield',type='o',col='blue')
#绘制样本自相关图
corr=acf(dat1$yield)
#自相关图显示该序列的自相关系数一直比较小，始终控制在2倍标准差范围内，可以认为该序列、
#自始至终都在零轴附近随机波动，不存在趋势和周期性，可以认为该序列是平稳的。
corr#样本自相关系数


#实验二
Index <- c(17,19,20,15,13,14,14,14,14,11,16,19,23,18,17,20,20,18,14)
Index <- ts(Index,frequency = 1,start = c(2010))
Index
plot(Index,type='o',lwd=2,col="blue",main="Index时间序列图")#画出时间序列图
corr2 <- acf(Index)#绘制样本自相关图，计算样本自相关系数
corr2
#观察自相关图，自相关系数衰减到0的速度十分缓慢，在较长的延迟期内，自相关系数先是正值，
#后变为负值，呈现明显的三角对称性，可以看出这是一个非平稳序列。

#判断时间序列的纯随机性
Box.test(Index,lag = 6,type = "Box-Pierce")
#P值为0.1769，在显著性水平为0.05下，接受原假设，该序列是白噪声序列

#构造检验统计量——Q统计量（n=19，样本容量小）
#n=19#序列观测期数
#m=6#延迟期数
#Q=n*sum((corr2$acf^2)[1:m])
#p=1-pchisq(Q,m,ncp=0,lower.tail = T,log.p = F)
#p


#实验三
x <- c(97,130,156.5,135.2,137.7,180.5,205.2,190,188.6,196.7,180.3,210.8,196,223,238.2,263.5,292.6,
            317,335.4,327,321.9,353.5,397.8,436.8,465.7,476.7,462.6,460.8,501.8,501.5)
x <- ts(x,start = 1989)
x
plot(x,type="o",col="green",lwd=1.5)
#时序图有上升趋势，明显不是平稳的
corr3 <- acf(x)
#根据自相关图来看，明显不是平稳的
corr3$acf
Box.test(x,lag = 6,type = "Box-Pierce")#拒绝原假设，不具有纯随机性，不是白噪声序列
#计算LB统计量来判断纯随机性
n3 <- length(x)
m=10
ss=0
for (i in 1:m) {
  s=((corr3$acf^2)[i])/(n3-i)
  ss=ss+s
}
ss
LB=n3*(n3+2)*ss
LB
P <- 1-pchisq(LB,m,ncp=0,lower.tail = T,log.p = F)
P
#P值为0，拒绝原假设，因此该序列不具有纯随机性
y <- c()
for (i in 1:n3) {
  y[i] <- x[i]-x[i-1]
}
y <- na.omit(y)#去NA
n4 <- length(y)
plot(y,type = "o",col="blue",lwd=1.8,main = "y-时序图",xlab="year")
#根据查分后的时序图，差分值都在10附近随机波动，没有明显的趋势和周期性，基本可以视为平稳序列
corr4 <- acf(y)
corr4$acf
Box.test(y,lag = 6,type = 'Box-Pierce')#接受原假设
#自相关图显示该序列的自相关系数一直比较小，始终控制在2倍标准差范围内，可以认为该序列、
#自始至终都在零轴附近随机波动，这是随机性非常强的平稳时间序列通常具有的自相关图性质。
#该序列具有平稳性和纯随机性

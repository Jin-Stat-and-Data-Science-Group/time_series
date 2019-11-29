#习题6.2
dat1 <- read.csv("C:/习题6.2.csv",sep=",",header = F)
dat1 <- as.vector(t(dat1))
output <- ts(dat1[1:38])
rain <- ts(dat1[45:82])
#(1)单位根检验
for(k in 1:3){
  t1 <- ur.df(output,type = c("drift"),lag=k)
  print(summary(t1))
}#谷物产量序列是有常数均值的平稳序列
for(l in 1:3){
  t2 <- ur.df(rain,type=c("drift"),lag=l)
  print(summary(t2))
}#降雨量序列是有常数均值的平稳序列
#(2)检验
for(k in 1:3){
  print(Box.test(output,lag=k))
}#序列为白噪声
for(l in 1:3){
  print(Box.test(rain,lag=l))
}#序列为白噪声
#(3)协整检验
fm1 <- lm(output~rain)
r1 <- ts(fm1$residuals)
for(k in 1:3){
  t3 <- ur.df(r1,type=c("none"),lag=k)
  print(summary(t3))
}#残差序列为平稳序列，所以两者之间具有协整关系

#习题6.4
dat2 <- read.csv("C:/习题6.4.csv",sep=",",header = F)
export <- ts(dat2[,2],start=1950)
import <- ts(dat2[,3],start=1950)
#(1)单位根检验
for(k in 1:3){
  t4 <- ur.df(export,type = c("trend"),lag=k)
  print(summary(t4))
}#出口总额序列非平稳
for(l in 1:3){
  t5 <- ur.df(import,type = c("trend"),lag=l)
  print(summary(t5))
}#进口总额序列非平稳
#(2)模型拟合
e1 <- log(export)
i1 <- log(import)
t <- c(1:59)
fm2 <- lm(e1~t)
fm3 <- lm(i1~t)
#(3)协整检验
fm4 <- lm(export~import)
r2 <- fm4$residuals
for(k in 1:3){
  t6 <- ur.df(r2,type=c("none"),lag=k)
  print(summary(t6))
}#残差序列为平稳序列，所以两者之间具有协整关系
#(4)残差修正模型
ecm <- fm4$residuals[1:58]
fm5 <- lm(diff(export)~0+diff(import)+ecm)
summary(fm5)
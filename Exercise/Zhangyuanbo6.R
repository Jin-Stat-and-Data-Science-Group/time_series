#################第九次上机实验##################
rm(list=ls())
setwd('D:/github_repo/time_series/')
#习题6.2
dat<- read.table('习题数据、案例数据、R代码/习题数据/习题6.2数据.txt',fill = T)
grain <- dat[2:5,]
grain <- as.numeric(t(grain))
grain <- ts(na.omit(grain))
rain <- dat[7:10,]
rain <- as.numeric(t(rain))
rain <- ts(na.omit(rain))

#ADF检验
library(fBasics)
library(fUnitRoots)
for (i in 1:3) print(adfTest(grain,lag=i,type='nc'))
for (i in 1:3) print(adfTest(grain,lag=i,type='c'))
##检验结果显示，谷物产出序列是有常数均值的平稳序列，该平稳序列1阶自相关（ADF的P值=0.01）
#对降雨量进行ADF检验
for (i in 1:3) print(adfTest(rain,lag=i,type='nc'))
for (i in 1:3) print(adfTest(rain,lag=i,type='c'))
##检验结果显示，降雨量序列是有常数均值的平稳序列，该平稳序列1阶自相关（ADF的P值=0.01）

#白噪声检验
for(i in 1:6) print(Box.test(grain,lag=i))
##不拒绝原假设，说明谷物产出是白噪声序列
for(i in 1:6) print(Box.test(rain,lag=i))
##不拒绝原假设，说明降雨量是白噪声序列

#协整检验
fit <- lm(grain~rain)
summary(fit)  #回归模型
r <- ts(fit$residuals)
for (i in 1:3) print(adfTest(r,lag=i,type='nc')) #残差序列单位根检验
##具有协整关系，回归模型为：grain=23.5521+0.7755*rain

#习题6.4
dat1<- read.table('习题数据、案例数据、R代码/习题数据/习题6.4数据.txt',header = T,sep = "\t",encoding = "UTF-8",skipNul = T)
colnames(dat1)=c("年份","出口总额","进口总额")
export <- dat1[,2]
export <- ts(export,start = 1950)
import <- dat1[,3]
import <- ts(import,start = 1950)
c3=min(export,import)
c4=max(export,import)
plot(export,ylim=c(c3,c4))
lines(import,col="red")

#出口额ADF检验
for (i in 1:3) print(adfTest(export,lag=i,type='nc'))
for (i in 1:3) print(adfTest(export,lag=i,type='c'))
for (i in 1:3) print(adfTest(export,lag=i,type='ct'))
##出口额序列不平稳
#进口额ADF检验
for (i in 1:3) print(adfTest(import,lag=i,type='nc'))
for (i in 1:3) print(adfTest(import,lag=i,type='c'))
for (i in 1:3) print(adfTest(import,lag=i,type='ct'))
##进口额序列不平稳
##从时序图可以看出，序列有递增趋势，取对数后作1阶差分，再进行单位根检验
lnx<- log(export)
lny <- log(import)
dif.lnx <- diff(lnx)
dif.lny <- diff(lny)
plot(dif.lnx)
lines(dif.lny,col=2)  #时序图
for (i in 1:3) print(adfTest(dif.lnx,lag=i,type='nc')) 
##出口额对数差分序列是无常数均值的平稳序列，该平稳序列1阶自相关（ADF的P值=0.01215）
for (i in 1:3) print(adfTest(dif.lny,lag=i,type='nc')) 
##进口额对数差分序列是无常数均值的平稳序列，该平稳序列1阶自相关（ADF的P值=0.01）

#对出口额对数序列拟合ARIMA(1,1,0)
lnx.fit <- arima(lnx,order = c(1,1,0))
lnx.fit
for (i in 1:6)  print(Box.test(lnx.fit$residuals,lag =i)) #残差白噪声检验
##不拒绝原假设，说明模型显著，模型为lnxt-lnxt-1=et/(1-0.6825*B)
lny.fit <- arima(lny,order = c(1,1,0))
lny.fit
for (i in 1:6)  print(Box.test(lny.fit$residuals,lag =i)) #残差白噪声检验
##不拒绝原假设，说明模型显著，模型为lnyt-lnyt-1=et/(1-0.5998*B)

#3、序列协整关系判断
fit <- lm(lny~lnx)#构造回归模型
summary(fit)
##回归模型为：lnyt=0.063935+0.984121*lnxt+et
r <- ts(fit$residuals) #残差序列单位根检验
for (i in 1:3) print(adfTest(r,lag=i,type='nc')) 
##结果显示，进口额与出口额具有协整关系,回归残差序列属于无常数均值1阶自相关平稳序列

#误差修正模型
ECM <- fit$residuals[1:58]
dif.fit <- lm(dif.lny~0+dif.lnx+ECM)
summary(dif.fit)
##误差修正模型为:detalnyt= 1.02006*detalnxt-0.31775*ECMt-1
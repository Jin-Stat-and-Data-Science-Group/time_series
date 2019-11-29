#unit2

#2.2.1 时序图
yield<-c(15.2,16.9,15.3,14.9,15.7,15.1,16.7)
yield<-ts(yield,start = 1884)
plot(yield)
#散点图
plot(yield,type="p")
#点线图
plot(yield,type = "o")
#符号参数
plot(yield,type = "o",pch=17)
#连线类型参数
plot(yield,lty=2)
#线的宽度参数
plot(yield,lwd=2)
#添加文本
plot(yield,main = "1884-1890年英格兰和威尔士地区小麦平均亩产量",xlab = "年份",ylab="亩产量")
#指定坐标轴范围
#指定输出横轴范围
plot(yield,xlim = c(1886,1890))
#指定输出纵轴范围
plot(yield,ylim=c(15,16))
#添加参照线
#添加一条垂线
plot(yield)
abline(v=1887,lty=2)
#添加多条垂直参照线
plot(yield)
abline(v=c(1885,1889),lty=2)
#添加水平线
plot(yield)
abline(h=c(15.5,16.5),lty=2)
#绘制序列自相关图
acf(yield)

#例2—1
sha<-read.table("E:/R/data/file4.csv",sep=",",header = T)
output<-ts(sha$output,start=1964)
plot(output)

#例2—2
a<-read.table("E:/R/data/file5.csv",sep=",",header = T)
milk<-ts(a$milk,start = c(1962,1),frequency = 12)
plot(milk)

#例2—3
b<-read.table("E:/R/data/file6.csv",sep=",",header = T)
temp<-ts(b$temp,start = 1949)
plot(temp)

#例2—1续
acf(output,lag=25)

#例2—2续
acf(milk)

#例2—1续(1)
acf(temp)

#例2—4
white_noise<-rnorm(1000)
white_noise<-ts(white_noise)
plot(white_noise)

#例2—4续(1)
acf(white_noise)

#例2—4续(2)
Box.test(white_noise,lag = 6)
Box.test(white_noise,lag=12)
for(i in 1:2)print(Box.test(white_noise,lag=6*i))

#例2—3续(2)
for(i in 1:2)print(Box.test(temp,lag=6*i))

#例2—5
c<-read.table("E:/R/data/file7.csv",sep=",",header = T)
prop<-ts(c$prop,start = 1950)
plot(prop)
acf(prop)
for(i in 1:2)print(Box.test(prop,lag=6*i))


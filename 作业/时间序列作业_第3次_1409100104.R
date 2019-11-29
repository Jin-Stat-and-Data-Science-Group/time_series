#第一题
#模拟函数 作图
set.seed(4)
xt<-arima.sim(n=200,list(ar=0.6),sd=0.5)
ts.plot(xt)
#判断为平稳序列
#写出格林函数前10项
G<-vector()
phi.1<-0.6
G0<-1
G1<-0.6
G2<-0.36
G[1:3]<-c(G0,G1,G2)
for (i in 3:10)  {
  G[i+1]<-sum(phi.1*G[i:(i-2)])
}
#计算方差
vxt<-sum(G^2*(0.5^2))
#计算自协方差前十项
gamma1<-vector()
for(i in 1:11)
{gamma1[1]=0.5^2/(1-0.6^2)
gamma[i]=0.6^i*0.5^2/(1-0.6^2)}
#acf前100项
acfxt=acf(xt,lag=10)$acf

#第二题
#模拟函数 作图
set.seed(4)
xt<-arima.sim(n=200,list(ar=c(1.2,-0.5)),sd=1.5)
ts.plot(xt)
#判断为平稳序列
#格林函数
G<-vector()
phi.2<-c(1.2,-0.5)
G0<-1
G1<-1.2
G2<-1.2*0.7-0.5*1
G[1:3]<-c(G0,G1,G2)
for (i in 3:10)  {
  G[i+1]<-sum(phi.2*G[i:(i-2)])
}
#求方差
vxt<-sum(G^2*(1.5^2))
#计算自协方差前十项
gammax2=vector()
for(i in 3:11)
{gammax2[1]=((1+0.5)/((1-0.5)*(1-1.2+0.5)*(1+1.2+0.5)))*1.5^2
gammax2[2]=1.2*gammax2[1]/(1+0.5)
gammax2[i]=1.2*gammax2[i-1]+-0.5*gammax2[i-2]}
#acf前100项
acfxt=acf(xt,lag=10)$acf

#第三题
#特征函数为(x-0.5)(x+0.2)(x-0.4)=0
#x^3-0.7x^2+0.02x+0.04=0
xt<-arima.sim(n=200,list(ar=c(0.7,-0.02,-0.04),sd=1))
#格林函数
G<-vector()
phi.3<-c(0.7,-0.02,-0.04)
G0<-1
G1<-0.7
G2<-0.7*0.7-0.02*1
G[1:3]<-c(G0,G1,G2)
for (i in 3:10)  {
  G[i+1]<-sum(phi.3*G[i:(i-2)])
}
#求方差v(xt)
vxt<-sum(G^2*(1^2))
#计算协方差前十项
gammax3=vector()
gammax3[1]=(1/0.3)-vxt
gammax3[2]=(-0.2*0.4+0.5)*gammax3[1]/(0.5*0.4+0.4^2-0.2)
gammax3[3]=(0.5+0.4)*gammax3[2]+-0.2*gammax3[1]
for(i in 4:11)
{gammax3[i]=0.5*gammax3[i-1]+-0.2*gammax3[i-2]+0.4*gammax3[i-3]}
#acf前100项
acfxt=acf(xt,lag=10)$acf
#第一题
#ARMA(3,4)转化为MA(100)
phi<-c(0.4,0.07,-0.034,0.0024)
theta<-c(-2.1,1.46,-0.336)
G<-vector()
G0<-1
G1<-0.4
G2<-0.23
G3<--0.034*1+0.07*0.4+0.4*0.23
G[1:4]<-c(G0,G1,G2,G3)
for (i in 4:100)  {
  G[i+1]<-sum(phi*G[i:(i-2)])
}
ma.c<-G
ma.c[2:4]<-G[2:4]+theta
ma.c
#ARMA(3,4)转化为AR(100)
phi<-c(0.4,0.07,-0.034,0.0024)
theta<-c(-2.1,1.46,-0.336)
I<-vector()
I0<-1
I1<--2.1
I2<-2.1*2.1+1.46
I[1:3]<-c(I0,I1,I2)
for (j in 4:100)  {
  I[j]<-sum(theta*I[(j-1):(j-3)])
}
ar.c<-I
ar.c[2:5]<-I[2:5]-phi
ar.c

#第二题
#经计算得系数为-2，1.35，-0.36,0.0324
theta2 <- c(-2,1.35,-0.36,0.0324)
ma.acf <- rep(0,5)
ma.acf[1]=1
ma.acf[5]=(theta2[4])/(1+sum(theta2^2))
for(j in 2:4)
  ma.acf[j]=(theta2[j-1]+sum(theta2[1:(5-j)]*theta2[j:4]))/(1+sum(theta2^2))
ma.acf

#第三题
#经计算得系数为2.9,-3.2675,1.77625,-0.46125,0.045
phi2 <- c(2.9,-3.2675,1.77625,-0.46125,0.045)
rho <- ARMAacf(ar=phi2,lag.max=10)
my.ar.pacf <- function(k){
  D=matrix(rep(0,k^2),nrow=k)
  for(i in 1:k)
    D[i,(i:k)]=rho[1:(k-i+1)]
  D=D+t(D)
  diag(D)=1
  DK=D
  DK[,k]=rho[2:(k+1)]
  ar.pacf=det(DK)/det(D)
  ar.pacf
}
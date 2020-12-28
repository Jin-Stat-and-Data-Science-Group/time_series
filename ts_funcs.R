# 函数一：计算时间序列x延迟第k期的自相关系数ACF
ACF = function(x,k){
    n = length(x)
    x_bar = mean(x)
    rou_k = sum((x[1:(n-k)]-x_bar)*(x[(k+1):n]-x_bar))/sum((x-x_bar)^2)
    return(rou_k)
}

# 函数二：计算时间序列x延迟前k期的所有自相关系数ACFs
ACFs = function(x,k){
    rou = c()
    for(i in 1:k) rou[i] = ACF(x,i)
    return(rou)
}

# 函数三：Q.test
Q.test = function(x,k){
    n = length(x)
    rou_k = ACFs(x,k)
    Q = n*sum(rou_k^2)
    p.v = 1 - pchisq(Q, df = k)
    result = c(Q, p.v)
    return(data.frame(result,row.names = c('x-squared', 'p-value')))
}

# 函数四：LB.test
LB.test = function (x,k){
    n = length(x)
    rou_k = ACFs(x,k)
    nk = n - 1:k
    lb = rou_k^2/nk
    LB = n*(n+2)*sum(lb)
    p.v = 1 - pchisq(LB, df = k)
    result = c(LB, p.v)
    return(data.frame(result,row.names = c('x-squared', 'p-value')))
}

# 函数五：ARMA(p,q)模型的格林函数
Green = function(ar, ma, n){  # n为得到的系数最后项索引值
    g = 1
    p = length(ar)
    q = length(ma)
    if (n>=p) phi = c(ar, rep(0, n-p)) else phi = ar[1:n]
    if (n>=q) theta = c(ma, rep(0, n-q)) else theta = ma[1:n]
    for (k in 1:n) g[1+k] = sum(phi[k:1]*g) - theta[k]  # 逆向取向量
    return(g)
}

# 函数六：ARMA(p,q)模型的逆函数
InvFunc = function(ar,ma,n){
    I = 1
    p = length(ar)
    q = length(ma)
    if (n>=p) phi = c(ar,rep(0,n-p)) else phi = ar[1:n]
    if (n>=q) theta = c(ma,rep(0,n-q)) else theta = ma[1:n]
    for (k in 1:n) I[1+k] = sum(theta[k:1]*I) - phi[k]
    return (I)
  }

# 函数七：计算时间序列x延迟第k期的偏自相关系数PACF
PACF = function(x,k){
    D = matrix(0,k,k)
    rho = c(0,ACFs(x,k))
    for (i in 1:k){
        D[i,(i:k)] = rho[1:(k-i+1)]}
    Dk = t(D) + D
    diag(Dk) = 1
    r = matrix(ACFs(x,k),k,1)
    phi_kk = tail(solve(Dk,r),1)
    return(phi_kk)
}

# 函数八：计算时间序列x延迟前k期的所有偏自相关系数PACFs
PACFs = function(x,k){
    pacfs = c()
    for (i in 1:k){
        pacfs[i] = PACF(x,i)
    }
    return (pacfs)
}

# 函数九：ARMA模型序列预测
ARMAForecast = function(arima.model, h, alpha=0.05){ #arima.model为arima函数输出结果
  ar = arima.model$model$phi
  ma = -arima.model$model$theta
  p = length(ar)
  q = length(ma)
  green = Green(ar=ar, ma=ma, n=(h-1))
  sigma2 = arima.model$sigma2
  epsilon = arima.model$residuals
  var.et = c() #预测方差
  res = get(arima.model$series)
  for (i in 1:h){
    if (i>q) {
      len = length(res)
      res = append(res, sum(ar * res[len:(len-p+1)]))
    } else {
      len = length(res)
      len2 = length(epsilon)
      res = append(res, (sum(ar * res[len:(len-p+1)]) - sum(ma[i:q]*epsilon[len2:(len2-(q-i))])))
    }
    var.et[i] = sum(green[1:i]^2)*sigma2
  }
  upper.interval = tail(res,h) + qnorm((1-alpha/2),mean=0,sd=1)*sqrt(var.et)
  lower.interval = tail(res,h) - qnorm((1-alpha/2),mean=0,sd=1)*sqrt(var.et)
  result = data.frame('Point Forecast' = tail(res,h), 'Lower' = lower.interval, 'Upper' = upper.interval)
  return (result)
}

# 函数十：ARIMA模型的格林函数
### 函数说明：ar为PhiB各项系数，ma为ThetaB各项系数，如PhiB=1-0.8*B，则ar=c(1,-0.8)，依此类推
ArimaGreen = function(ar,ma,d,k){
    p = length(ar)
    q = length(ma)
    require(polynom)
    par = as.polynomial(ar)
    pma = as.polynomial(ma)
    b = c()
    for(i in 1:d) b[i] = (-1)^(d+i-1)*choose(d,i)
    pb = as.polynomial(c(1,b))
    phi = -unclass(par*pb)[-1]
    if(k > p) phi = c(phi,rep(0,k-p)) else phi = c(phi)
    if(k > q) theta = c(ma[-1],rep(0,k-q+1)) else theta = c(ma[-1])
    psi = 1
    for(j in 1:k) psi[1+j] = sum(phi[j:1]*psi) + theta[j]
    return(psi)
}
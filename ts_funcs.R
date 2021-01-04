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
    D=matrix(0,k,k)
    if (k==1) D=1
    else {
        rho=c(rev(ACFs(x,(k-1))),1,ACFs(x,(k-1)))
        for (i in 1:k) D[(k:1),i]=rho[i:(i+k-1)]
    }
    r=matrix(ACFs(x,k),k,1)
    phi_kk=tail(solve(D,r),1)
    return(phi_kk)
}


# 函数八：计算时间序列x延迟前k期的所有偏自相关系数PACFs
PACFs = function(x,k){
    pacfs=c()
    for (i in 1:k) pacfs[i]=PACF(x,i)
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
  epsilon = c(arima.model$residuals, rep(0,h))
  var.et = c() #预测方差
  res = get(arima.model$series)
  for (i in 1:h){
      len = length(res)
      len2 = length(epsilon)
      res = c(res, (sum(ar * res[len:(len-p+1)]) - sum(ma*epsilon[((len2-h)+(i-1)):((len2-h)-(q-i))])))
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
    require(polynom)
    par = as.polynomial(ar)
    b = c()
    for(i in 0:d) b[i+1] = (-1)^i*choose(d,i)
    pb = as.polynomial(b)
    phi = -unclass(par*pb)[-1]
    Green(phi,-ma[-1],k)
}

# 函数十一：AR(p)的ADF检验函数

ADF = function (x, lag, type = c('nc', 'c', 'ct')) {
    x_diff = diff(x)    # x的一阶差分
    n = length(x)       # 样本数
    X_d = matrix(, n-lag, lag)   # x的一阶差分矩阵
    for (i in 1:lag) X_d[, i] = x_diff[(lag-i+1):(n-i)]
    y_diff = X_d[, 1]             # 构造回归模型因变量
    x.lag = x[lag:(n-1)]         # 自变量一；对应回归系数为ρ
    t = (lag+1):n
    if (lag > 1) {
        x_d.lag = X_d[, 2:lag]   # 自变量二；对应回归系数为β_j
        if (type == 'nc') {       # nc: 无常数均值，无趋势类型
            res = lm(y_diff ~ x.lag - 1 + x_d.lag)}     
        if (type == 'c') {        # c: 有常数均值，无趋势类型
            res = lm(y_diff ~ x.lag + 1 + x_d.lag)}
        if (type == 'ct') {       # ct: 有常数均值，有趋势类型
            res = lm(y_diff ~ x.lag + 1 + t + x_d.lag)}
    }
    else {        # lag=1时，x_d.lag不存在
        if (type == 'nc') res = lm(y_diff ~ x.lag - 1)
        if (type == 'c')  res = lm(y_diff ~ x.lag + 1)
        if (type == 'ct') res = lm(y_diff ~ x.lag + 1 + t)
    }
    res.sum = summary(res)
    if (type == "nc") ρ_loc = 1 else ρ_loc = 2
    adf = res.sum$coefficients[ρ_loc, 1] / res.sum$coefficients[ρ_loc, 2]     # ρ估计值 / ρ样本标准差 
    if (type == "nc") {
        table = rbind(c(-2.65, -2.26, -1.95, -1.6, -0.47, 0.92, 
            1.33, 1.7, 2.15), c(-2.62, -2.25, -1.95, -1.61, -0.49, 
            0.91, 1.31, 1.66, 2.08), c(-2.6, -2.24, -1.95, -1.61, 
            -0.5, 0.9, 1.29, 1.64, 2.04), c(-2.58, -2.24, -1.95, 
            -1.62, -0.5, 0.89, 1.28, 1.63, 2.02), c(-2.58, -2.23, 
            -1.95, -1.62, -0.5, 0.89, 1.28, 1.62, 2.01), c(-2.58, 
            -2.23, -1.95, -1.62, -0.51, 0.89, 1.28, 1.62, 2.01))}
    if (type == "c") {
        table = rbind(c(-3.75, -3.33, -2.99, -2.64, -1.53, 
            -0.37, 0, 0.34, 0.71), c(-3.59, -3.23, -2.93, -2.6, 
            -1.55, -0.41, -0.04, 0.28, 0.66), c(-3.5, -3.17, 
            -2.9, -2.59, -1.56, -0.42, -0.06, 0.26, 0.63), c(-3.45, 
            -3.14, -2.88, -2.58, -1.56, -0.42, -0.07, 0.24, 0.62), 
            c(-3.44, -3.13, -2.87, -2.57, -1.57, -0.44, -0.07, 
            0.24, 0.61), c(-3.42, -3.12, -2.86, -2.57, -1.57, 
            -0.44, -0.08, 0.23, 0.6))}
    if (type == "ct") {
        table = rbind(c(-4.38, -3.95, -3.6, -3.24, -2.14, -1.14, 
            -0.81, -0.5, -0.15), c(-4.16, -3.8, -3.5, -3.18, 
            -2.16, -1.19, -0.87, -0.58, -0.24), c(-4.05, -3.73, 
            -3.45, -3.15, -2.17, -1.22, -0.9, -0.62, -0.28), 
            c(-3.98, -3.69, -3.42, -3.13, -2.18, -1.23, -0.92, 
            -0.64, -0.31), c(-3.97, -3.67, -3.42, -3.13, 
            -2.18, -1.24, -0.93, -0.65, -0.32), c(-3.96, 
            -3.67, -3.41, -3.13, -2.18, -1.25, -0.94, -0.66, -0.32))}
    pvalue <- function(table, stat) {
        Ncol <- ncol(table)
        Size <- c(25, 50, 100, 250, 500, 1e+05)
        Percnt <- c(0.01, 0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975, 0.99)
        intplSize <- numeric(Ncol)
        for (j in 1:Ncol) intplSize[j] <- approx(Size, table[, 
            j], n-1, rule = 2)$y
        approx(intplSize, Percnt, stat, rule = 2)$y
    }
    PVAL = pvalue(table, adf)
    ADFTest = c(lag, type, round(adf, 4), round(PVAL, 4))   # 保留四位小数
    return(data.frame(ADFTest, row.names = c('Lag:', 'Type:', 'ADF:', 'P.value:')))
}

# 函数十二：arima系数检验函数

arima_test = function(result,P){
    Coef = result$coef
    Se = sqrt(diag(result$var.coef))
    N = length(result$residuals)
    m = length(result$coef)-1
    t_pvalue = Coef/Se
    t_test = pt(abs(t_pvalue),df=(N-m),lower.tail = F)
    return(t_test<P)
}
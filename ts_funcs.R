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

#函数六：ARMA(p,q)模型的逆函数
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
# 函数一：计算时间序列x延迟第k期的自相关系数ACF，并在Q.test和LB.test中使用
ACF = function(x,k){
    n = length(x)
    x_bar = mean(x)
    rou = c()
    for(i in 1:k) rou[i] = sum((x[1:(n-i)]-x_bar)*(x[(i+1):n]-x_bar))/sum((x-x_bar)^2)
    return(rou[k])
}

# 函数二：计算时间序列x延迟前k期的所有自相关系数ACFs
ACFs = function(x,k){
    n = length(x)
    x_bar = mean(x)
    rou = c()
    for(i in 1:k) rou[i] = sum((x[1:(n-i)]-x_bar)*(x[(i+1):n]-x_bar))/sum((x-x_bar)^2)
    return(rou)
}

# 函数三：Q.test
Q.test = function(x,k){
    n = length(x)
    rou_k = ACFs(x,k)
    Q = n*sum(rou_k^2)
    p.v = 1 - pchisq(Q, df=k)
    result = c(Q ,p.v)
    return(data.frame(result,row.names = c('x-squared', 'p-value')))
}

# 函数四：LB.test
LB.test = function (x,k){
    n = length(x)
    rou_k = ACFs(x,k)
    lb = c()
    for (i in 1:k) {
        lb[i]=rou_k[i]^2/(n-i)
    }
    LB = n*(n+2)*sum(lb)
    p.v = 1 - pchisq(LB, df = k)
    result = c(LB, p.v)
    return(data.frame(result,row.names = c('x-squared', 'p-value')))
}
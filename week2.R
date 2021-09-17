data('ceosal1')

y = log(ceosal1$salary) 
x = log(ceosal1$sales)
plot(x, y)

sxy = sum((x-mean(x)) * (y-mean(y)))
sxx = sum((x-mean(x))^2)
beta1 = sxy/sxx
beta0 = mean(y) - mean(x) * beta1
cov(x,y)/var(x)
beta = c(intercept = beta0, slope = beta1)

yhat = beta0 + beta1 * x
uhat = y - yhat

TSS = sum((y-mean(y))^2)
RSS = sum(uhat^2)
ESS = TSS - RSS
R2 = ESS/TSS

n = dim(ceosal1)[1]

df = dim(ceosal1)[1] - 2

sigma2 = RSS/df
se_beta1 = sqrt(sigma2/sxx)
se_beta0 = sqrt(sigma2*(1/n + mean(x)^2/sxx))

lmfit = lm(y~x)
summary(lmfit)